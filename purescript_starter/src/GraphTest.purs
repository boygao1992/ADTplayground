module GraphTest where

import Data.List
import Data.Tuple
import Prelude

import Data.Graph (Graph(..), fromMap)
import Data.Map (Map, fromFoldable)

testArray :: Array (Tuple Int Int)
testArray = [Tuple 1 1]

testMap :: Map Int Int
testMap = fromFoldable testArray

