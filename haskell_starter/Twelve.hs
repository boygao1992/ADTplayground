module Twelve where

import Prelude
import qualified Data.Map.Strict as Map

matrix :: Map.Map (Int, Int) Char
matrix = Map.fromList [((0,0), 'a'), ((0,1), 'b'), ((0,2), 't'), ((0,3), 'g')
                      ,((1,0), 'c'), ((1,1), 'f'), ((1,2), 'c'), ((1,3), 's')
                      ,((2,0), 'j'), ((2,1), 'd'), ((2,2), 'e'), ((2,3), 'h')]

