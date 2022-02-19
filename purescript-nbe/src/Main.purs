module Main where

import Prelude
import Bidirectional as Bidirectional
import Effect (Effect)
import Effect.Console as Effect.Console

main :: Effect Unit
main = do
  Effect.Console.logShow
    Bidirectional.test
