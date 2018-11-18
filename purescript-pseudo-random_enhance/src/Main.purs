module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Random.PseudoRandom (randomSeed)
import Random.PseudoRandom.WithSeed (randomRsWithSeed)

main :: Effect Unit
main = do
  seed <- randomSeed
  log $ "seed0: " <> show seed
  let { values, seed : seed1 } = randomRsWithSeed 'a' 'z' (-20) seed
  log $ "values: " <> show values
  log $ "seed1: " <> show seed1
