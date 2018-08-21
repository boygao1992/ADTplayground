module TenFour where

import Prelude
import Data.Function (fix)

step :: (Int -> Integer) -> Int -> Integer
step _ 0 = 1
step _ 1 = 1
step f n = sum (map f [0..(n-1)])

memorize :: (Int -> a) -> Int -> a
memorize f = (map f [0..] !!)

solve :: Int -> Integer
solve = fix (memorize . step)
