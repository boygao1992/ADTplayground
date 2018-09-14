module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import AVar as AVar

main :: Effect Unit
main = do
  var <- AVar.new "foo"
  val1 <- AVar.tryRead var
  val2 <- AVar.tryRead var
  log $ show val1 <> show val2
