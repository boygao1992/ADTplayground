module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (Aff, Canceler, Error, makeAff, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either, either)

type Task x a = ExceptT x Aff a

resolve :: forall x a. a -> Task x a
resolve = pure

reject :: forall x a. x -> Task x a
reject = throwError

newTask
  :: forall x a
   . (( Either Error (Either x a) -> Effect Unit) -> Effect Canceler) -> Task x a
newTask =
  ExceptT <<< makeAff

toAff :: forall x a. Task x a -> Aff (Either x a)
toAff = runExceptT

-- class Choice => fanin
fork :: forall a b c. (a -> Aff c) -> (b -> Aff c) -> Task a b -> Aff c
fork f g t = do
  result <- toAff t
  either f g result

-- class Monad => bind
chain :: forall x a b. (a -> Task x b) -> Task x a -> Task x b
chain = (=<<)

main :: Effect Unit
main = do
  log "Hello sailor!"
