module Sixty where

import Prelude
import Data.Function (fix)

solve :: Int -> Int -> Integer
solve 1 1 = 1
solve 1 2 = 1
solve 1 3 = 1
solve 1 4 = 1
solve 1 5 = 1
solve 1 6 = 1
solve n m
  | n <= 0 || m <= 0 = 0
  | otherwise = sum $ map (solve (n-1)) (map (m - ) [1..6])




