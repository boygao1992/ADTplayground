module Data.VectorSpace where

import Prelude
import Data.Group
import Data.Semigroup.Commutative

class (Field a, Group v, Commutative v) <= VectorSpace v a | v -> a
  where
    zero :: v
    scalarMul :: a -> v -> v
    scalarDiv :: v -> a -> v
    add :: v -> v -> v
    sub :: v -> v -> v
    -- negate :: v -> v
    dot :: v -> v -> a
    -- norm :: v -> a
    -- normalize :: v -> v

infixr 9 scalarMul as *^
infixr 9 scalarDiv as ^/
infixl 6 add as ^+^
infixl 6 sub as ^-^
