module Data.Generic.Rep.Index where

import Prelude
import Type.Proxy (Proxy)

import Data.Lens (class Wander, Optic')
import Data.Lens.Index (class Index, ix)

class GenericIndex p s i o | p s i -> o where
  genericIndex :: Proxy i -> Optic' p s i -> o

instance genericIndexImpl
  :: ( Index i a b
    , Wander p
    )
  => GenericIndex p s i (a -> (p b b -> p s s))
  where
    genericIndex _ _i a = _i <<< ix a
