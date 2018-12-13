module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)

import Run (EFFECT, Run, liftEffect, runBaseEffect)
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

program :: forall r. Run ( state :: STATE AppState, effect :: EFFECT | r) Unit
program = do
  state <- State.get
  liftEffect $ log $ "current state: " <> show state
  State.modify _ { count = state.count + 1 }


main :: Effect Unit
main = do
  result <- runBaseEffect
    <<< (State.runState { count: 10 })
    $ do
    initalization
    program
  logShow result
