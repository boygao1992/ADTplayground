module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Regex.Runtime (tokenize, parse)

main :: Effect Unit
main = do
  logShow $ tokenize "abc*d.f.*g?h"

  logShow $ parse <<< tokenize $ "abc*d.f.*g?h"
