module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Regex.Runtime (tokenize)

main :: Effect Unit
main = do
  logShow $ tokenize "abc*d.f.*g?h"
