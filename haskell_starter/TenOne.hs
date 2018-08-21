module TenOne where

import Prelude

fibo :: [Integer]
fibo = 0 : 1 : zipWith (+) fibo (tail fibo)

solve :: Int -> Integer
solve = (fibo !!)
