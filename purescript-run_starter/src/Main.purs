module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Run (EFFECT, Run, liftEffect, runBaseEffect)
import Run.Except (EXCEPT, FAIL)
import Run.Except (throw, catch) as Except
import Run.State (STATE)
import Run.State (get, gets, modify, put, putAt, runState, runStateAt) as State

type AppState =
  { count :: Int }

type AppEffects =
  ( state :: STATE AppState
  , effect :: EFFECT
  )

initalization :: forall r. Run ( state :: STATE AppState | r) Unit
initalization = do
  State.put { count : 0 }

processor :: forall r. Run ( state :: STATE AppState, effect :: EFFECT | r) Unit
processor = do
  state <- State.get
  liftEffect $ log $ "current state: " <> show state
  State.modify _ { count = state.count + 1 }

validator :: forall r. Run ( state :: STATE AppState, except :: EXCEPT String | r) Unit
validator = do
  state <- State.get
  if state.count < 2
    then Except.throw "too small"
    else State.put { count: state.count - 1 }

main :: Effect Unit
main = do
        void
    <<< runBaseEffect
    <<< State.runState { count: 10 }
    <<< Except.catch (liftEffect <<< logShow)
    $ do
      initalization
      processor
      validator
