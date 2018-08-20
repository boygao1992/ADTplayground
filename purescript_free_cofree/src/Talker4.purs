module Talker4 where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Effect (Effect)
import Effect.Console (log)

data TalkerF r
  = Speak String r
  | Listen (String -> r)

type Talker a = Free TalkerF a

speak :: String -> Talker Unit
speak s = liftF $ Speak s unit

listen :: Talker String
listen = liftF $ Listen identity

run :: String -> Talker ~> Effect
run name = foldFree interpret where
  interpret :: TalkerF ~> Effect
  interpret (Speak s a) = do -- const a <$> log s
    log s
    pure a
  interpret (Listen k) = pure (k name)

echo :: Talker Unit
echo = do
  speak "Hello"
  speak "What is your name ?"
  name <- listen
  speak ("Nice to meet you, " <> name)

main :: Effect Unit
main = run "Jerry" echo
