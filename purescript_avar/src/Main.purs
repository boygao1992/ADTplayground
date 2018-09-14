module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.AVar as AVar

main :: Effect Unit
main = do
  log "Hello sailor!"
