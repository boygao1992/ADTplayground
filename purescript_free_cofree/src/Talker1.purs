module Talker1 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

data Talker
  = Speak String Talker
  | Listen (String -> Talker)
  | Done

program :: Talker
program
  = Speak "Hello"
  $ Speak "What is your name ?"
  $ Listen \name
  ->Speak ("Nice to meet you, " <> name)
  $ Done

run :: String -> Talker -> Effect Unit
run name =interpret
  where
    interpret = case _ of
      Speak str next -> do
        log str
        interpret next
      Listen reply -> do
        interpret $ reply name
      Done ->
        pure unit

main :: Effect Unit
main = run "Jeffry" program
