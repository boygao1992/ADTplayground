module FourTeen where

import Prelude
import Data.Function (fix)

solve :: (Int -> Integer) -> Int -> Integer
solve _ 0 = 0
solve _ 1 = 1
solve _ 2 = 2
solve _ 3 = 3
solve f n = maximum $ zipWith (*)
  (map f [1 .. (n `div` 2)])
  (map f [(n-1), (n-2) .. ((n+1) `div` 2)])

memorize :: (Int -> a) -> Int -> a
memorize f = (map f [0 ..] !!)

solution :: Int -> Integer
solution = fix (memorize . solve)
