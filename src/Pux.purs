module Pux
  ( App
  , Config
  , Update
  , EffModel
  , CoreEffects
  , noEffects
  , onlyEffects
  , fromSimple
  , mapState
  , mapEffects
  , renderToDOM
  , renderToString
  , start
  , toReact
  ) where

import Control.Monad.Aff (Aff, Canceler, later, launchAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl, sequence_)
import Data.List (List(Nil), singleton, (:), reverse, fromFoldable)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, ($), (<<<), bind, map, pure)
import Pux.Html (Html)
import React (ReactClass)
import Signal (Signal, (~>), mergeMany, foldp, runSignal)
import Signal.Channel (CHANNEL, Channel, channel, subscribe, send)

-- | Start an application. The resulting html signal is fed into `renderToDOM`.
-- |
-- | ```purescript
-- | main = do
-- |   app <- start
-- |     { update: update
-- |     , view: view
-- |     , initialState: initialState
-- |     , inputs: [] }
-- |
-- |   renderToDOM "#app" app.html
-- | ```
start :: forall state action eff.
         Config state action eff ->
         Eff (CoreEffects eff) (App state action)
start config = do
  actionChannel <- channel Nil
  let actionSignal = subscribe actionChannel
  -- Create a single signal to update the app's state.
  -- Merge external actions into internal `actionSignal`.
  let inputSignal :: Signal (List action)
      inputSignal = unsafePartial $ fromJust $ mergeMany $
        -- Reverse to emulate left-associative functions
        --   and prioritize external actions.
        reverse (actionSignal : map (map singleton) (fromFoldable config.inputs))
  -- Initial state-update model on which to iterate. Models the result of a state update.
  let effModelSignal :: Signal (EffModel state action eff)
      effModelSignal = foldp foldActions (noEffects config.initialState) inputSignal
  -- Track state.
  let stateSignal :: Signal state
      stateSignal = effModelSignal ~> _.state
  -- Html, which is a virtual-dom tree. Pass to `renderToDOM` to render in browser.
  let htmlSignal :: Signal (Html action)
      htmlSignal = stateSignal ~> formatForReact actionChannel <<< config.view
  -- Run effects, sending their resulting Actions to the `actionChannel`.
  let effectsSignal = effModelSignal ~> map (launchAffect actionChannel) <<< _.effects
  runSignal $ effectsSignal ~> sequence_
  -- Return externally-relevant data.
  pure $ { html: htmlSignal, state: stateSignal, actionChannel: actionChannel }
    where
      formatForReact :: Channel (List action) -> Html action -> Html action
      formatForReact actionChannel html =
        formatTreeForReact (send actionChannel <<< singleton) html
      foldActions :: List action -> EffModel state action eff -> EffModel state action eff
      foldActions actions effModel =
        foldl foldState (noEffects effModel.state) actions
      foldState :: EffModel state action eff -> action -> EffModel state action eff
      foldState effModel action = config.update action effModel.state
      launchAffect :: Channel (List action)
          -> Aff (CoreEffects eff) action
          -> Eff (CoreEffects eff) (Canceler (channel :: CHANNEL | eff))
      launchAffect actionChannel affect =
        launchAff $ unsafeCoerceAff do
          action <- later affect
          liftEff $ send actionChannel (singleton action)

-- Render Pux elements to a virtual-dom tree.
foreign import formatTreeForReact :: forall a eff. (a -> Eff eff Unit) -> (Html a) -> (Html a)

-- | The configuration of an app consists of update and view functions along
-- | with an initial state.
-- |
-- | The `update` and `view` functions describe how to step the state and view
-- | the state.
-- |
-- | The `inputs` array enables a Pux app to handle externally-sourced actions.
-- | These are merged into the app's internal action signal.
type Config state action eff =
  { update :: Update state action eff
  , view :: state -> Html action
  , initialState :: state
  , inputs :: Array (Signal action)
  }

-- | The set of effects every Pux app needs to allow through when using `start`.
-- | Extend this type with your own app's effects, for example:
-- |
-- | ```purescript
-- | type AppEffects = (console :: CONSOLE, dom :: DOM)
-- |
-- | main :: State -> Eff (CoreEffects AppEffects) (App State Action)
-- | main state = do
-- |   -- ...
-- | ```
type CoreEffects eff = (channel :: CHANNEL, err :: EXCEPTION | eff)

-- | An `App` consists of three signals:
-- |
-- | * `html` – A signal of `Html` representing the current view of your
-- |   app. This should be fed into `renderToDOM`.
-- |
-- | * `state` – A signal representing the application's current state.
-- |
-- | * `actionChannel` - A signal of actions the application handles.
type App state action =
  { html :: Signal (Html action)
  , state :: Signal state
  , actionChannel :: Channel (List action)
  }

-- | Synonym for an update function that returns state and an array of
-- | asynchronous effects that return an action.
type Update state action eff = action -> state -> EffModel state action eff

-- | `EffModel` models the result of updating the app's state.
-- | It enables simply setting the new state as well as defining
-- |   asynchronous means of updating the state, which consists of
-- |   producing an action the app can handle from an asynchronous effect.
type EffModel state action eff =
  { state :: state
  , effects :: Array (Aff (CoreEffects eff) action)
  }

-- | Create an `Update` function from a simple step function.
fromSimple :: forall s a eff. (a -> s -> s) -> Update s a eff
fromSimple update = \action state -> noEffects $ update action state

-- | Create an `EffModel` with no effects from a given state.
noEffects :: forall state action eff. state -> EffModel state action eff
noEffects state = { state: state, effects: [] }

onlyEffects :: forall state action eff.
               state -> Array (Aff (CoreEffects eff) action) -> EffModel state action eff
onlyEffects state effects = { state: state, effects: effects }

-- | Map over the state of an `EffModel`.
mapState :: forall sa sb a e. (sa -> sb) -> EffModel sa a e -> EffModel sb a e
mapState a2b effmodel =
  { state: a2b effmodel.state, effects: effmodel.effects }

-- | Map over the effectful actions of an `EffModel`.
mapEffects :: forall s a b e. (a -> b) -> EffModel s a e -> EffModel s b e
mapEffects action effmodel =
  { state: effmodel.state, effects: map (map action) effmodel.effects }

foreign import renderToDOM :: forall a eff. String -> Signal (Html a) -> Eff eff Unit

foreign import renderToString :: forall a eff. Signal (Html a) -> Eff eff String

-- | Return a ReactClass from a Pux component's html signal.
foreign import toReact :: forall a props eff.
                          Signal (Html a) ->
                          Eff eff (ReactClass props)
