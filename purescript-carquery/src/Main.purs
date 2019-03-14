module Main where

import Prelude

import Effect (Effect)
import CarQuery.Effect.Trims (sampling)

main :: Effect Unit
main = do
  sampling sampleNum inputBasePath sampleOutputPath

  where
    inputBasePath = "./data/CarQuery/trims/"
    outputCollectionPath = "./data/CarQuery/output.json"

    sampleOutputPath = "./data/CarQuery/sample.json"

    sampleNum = 2

