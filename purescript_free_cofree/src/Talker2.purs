module Talker2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Free (Free(..))

data TalkerF next
  = Speak String next
  | Listen (String -> next)

type Talker a = Free TalkerF a

program :: Talker Unit
program
  = Roll $ Speak "Hello"
  $ Roll $ Speak "What is your name ?"
  $ Roll $ Listen \name
  ->Roll $ Speak ("Nice to meet you, " <> name)
  $ Pure unit

run :: forall a. String -> Talker a -> Effect Unit
run name =interpret
  where
    interpret = case _ of
      Roll (Speak str next) -> do
        log str
        interpret next
      Roll (Listen reply) -> do
        interpret $ reply name
      Pure _ ->
        pure unit

main :: Effect Unit
main = run "Euler" program
