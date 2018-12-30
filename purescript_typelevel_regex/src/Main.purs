module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Regex.Runtime (tokenize, parse, recognize)

main :: Effect Unit
main = do
  logShow $ tokenize "abc*d.f.*g?h"

  logShow $ parse <<< tokenize $ "abc*d.f.*g?hi{3}j.?k"

  -- | NumFixed
  logShow $ parse <<< tokenize $ "a{3}b"
  logShow $ parse <<< tokenize $ ".{3}a"
  logShow $ parse <<< tokenize $ "a{3b}c"
  logShow $ parse <<< tokenize $ "a{3*}b"
  logShow $ parse <<< tokenize $ "a3}b"
  logShow $ parse <<< tokenize $ "a{3b"
  logShow $ parse <<< tokenize $ "a{{3}b"

  -- | NumMaybe
  logShow $ parse <<< tokenize $ "a?"
  logShow $ parse <<< tokenize $ ".?"
  logShow $ parse <<< tokenize $ "*?"
  logShow $ parse <<< tokenize $ "?"

  -- | Star
  logShow $ parse <<< tokenize $ "a*"
  logShow $ parse <<< tokenize $ ".*"
  logShow $ parse <<< tokenize $ "?*"
  logShow $ parse <<< tokenize $ "*"

  logShow $ do
    patterns <- parse <<< tokenize $ "a*bcd*e.g.?h.?j.*kl{3}m"
    pure $ recognize patterns "aaaabcefghij12345klllm"

  logShow $ do
    patterns <- parse <<< tokenize $ "a*a*"
    pure $ recognize patterns "aaaaaaaaa"
