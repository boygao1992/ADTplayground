module Tailrec where

import Prelude

import Data.Tuple (Tuple(..))

pow :: Int -> Int -> Int
pow n p = go (Tuple 1 p)
  where
    go (Tuple acc 0) = acc
    go (Tuple acc p) = go (Tuple (acc * n) (p - 1))
