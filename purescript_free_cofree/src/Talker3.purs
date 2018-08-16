module Talker3 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Free (Free(..))

data TalkerF next
  = Speak String next
  | Listen (String -> next)

derive instance functorTalkerF :: Functor TalkerF

type Talker a = Free TalkerF a

speak :: String -> Talker Unit
speak str = Roll $ Speak str done

listen :: Talker String
listen = Roll $ Listen pure

done :: Talker Unit
done = Pure unit

program :: Talker Unit
program = do
  speak "Hello"
  speak "What is your name ?"
  name <- listen
  speak ("Nice to meet you, " <> name)

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
