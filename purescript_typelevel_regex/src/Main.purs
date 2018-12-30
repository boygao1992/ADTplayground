module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Regex.Runtime (tokenize, parse, recognize)

main :: Effect Unit
main = do
  logShow $ tokenize "abc*d.f.*g?h"

  logShow $ parse <<< tokenize $ "abc*d.f.*g?hi{3}j.?k"

  logShow $ parse <<< tokenize $ "a{3b}c"

  logShow $ do
    patterns <- parse <<< tokenize $ "a*bcd*e.g.?h.?j.*k"
    pure $ recognize patterns "aaaabcefghij12345k"

  logShow $ do
    patterns <- parse <<< tokenize $ "a*a*"
    pure $ recognize patterns "aaaaaaaaa"
