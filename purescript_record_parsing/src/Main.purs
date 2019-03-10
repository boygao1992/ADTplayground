module Main where

import Prelude

import Data.Lens.Index.Recordable (toRecord)
import Data.Map (fromFoldable) as Map
import Data.Tuple (Tuple (..))
import Effect (Effect)
import Effect.Console (logShow)

type SampleRow =
  { name :: String
  , age :: Int
  , weight :: Number
  , height :: Number
  , searchable :: Boolean
  }

main :: Effect Unit
main = do
  logShow
  $ toRecord
    ( Map.fromFoldable
      [ Tuple "name" "wenbo"
      , Tuple "age" "1"
      , Tuple "weight" "2" -- Int to Number
      , Tuple "height" "3.000"
        -- missing field "searchable", `fill`ed by `empty`
      ]
    ) :: SampleRow -- { age: 1, height: 3.0, name: "wenbo", searchable: false, weight: 2.0 }
