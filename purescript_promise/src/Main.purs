module Main where

import Prelude hiding (add)
import Control.Monad.Cont (Cont, runCont)
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Console (logShow, log)


add :: forall r. Int -> Int -> Cont r Int
add x y = pure $ x + y

square :: forall r. Int -> Cont r Int
square x = pure $ x * x

pathagoras :: forall r. Int -> Int -> Cont r Int
pathagoras x y = do
  x2 <- square x
  y2 <- square y
  add x2 y2

both :: String -> String -> ContT Unit Effect String
both person1 person2 = ContT $ \rest_of_program -> do
  rest_of_program person1
  rest_of_program person2

prog :: ContT Unit Effect Unit
prog = do
  lift $ log "Show names:"
  name <- both "wenbo" "robot"
  lift $ log name -- rest of the program runs twice, once for each person

main :: Effect Unit
main = do
  logShow $ runCont (pathagoras 3 4) identity

  runContT prog pure
