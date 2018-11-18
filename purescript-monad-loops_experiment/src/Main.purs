module Main where

import Prelude

import Control.Monad.Loops ()
import Control.Monad.Rec.Loops ()
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello sailor!"
