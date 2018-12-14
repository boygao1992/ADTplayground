module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Run (EFFECT, Run, liftEffect, runBaseEffect)
import Run.Except (EXCEPT)
import Run.Except (throw, catch) as Except
import Run.State (STATE)
import Run.State (get, modify, put, runState) as State
import Run.Reader (READER)
import Run.Reader (ask, runReader) as Reader
import Run.Writer (WRITER)
import Run.Writer (tell, runWriter) as Writer

type AppConfig =
  { seed :: Int }

type AppState =
  { count :: Int
  , step :: Int
  }

type AppEffects =
  ( state :: STATE AppState
  , reader :: READER AppConfig
  , effect :: EFFECT
  )

type AppLog = Array String

initalization :: forall r. Run ( state :: STATE AppState, reader :: READER AppConfig, writer :: WRITER AppLog | r) Unit
initalization = do
  env <- Reader.ask
  State.put { count : env.seed * 10, step : 1 }
  Writer.tell [ "initialization: success" ]

processor :: forall r. Run ( state :: STATE AppState, effect :: EFFECT, writer :: WRITER AppLog | r) Unit
processor = do
  state <- State.get
  liftEffect $ log $ "current state: " <> show state
  State.modify _ { count = state.count + 1 }
  State.modify _ { step = state.step + 1 }
  Writer.tell [ "processor: success" ]

validator :: forall r. Run ( state :: STATE AppState, except :: EXCEPT String, writer :: WRITER AppLog | r) Unit
validator = do
  state <- State.get
  State.modify _ { step = state.step + 1 }
  if state.count < 2
    then do
      Writer.tell [ "validator: failure" ]
      Except.throw "too small"
    else do
      State.modify _ { count = state.count - 1 }
      Writer.tell [ "validator: success" ]

main :: Effect Unit
main = do
  result <- runBaseEffect
    <<< State.runState { count: 0, step: 0 }
    <<< Reader.runReader { seed: 0 }
    <<< Writer.runWriter
    <<< Except.catch (liftEffect <<< logShow)
      $ do
          initalization
          processor
          validator

  logShow result
