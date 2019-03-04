module Data.HObject.Primitive where

import Data.Tuple (Tuple(..))

-- | Sum types comprising of primitive types can be annoying to work with in APIs because they require data
-- | constructors. This module provides a helper method to construct Tuples with primitive values so that you
-- | don't need to include their constructors. This technique works for all nullary types but is really only useful
-- | for primitives. To use mkTuple (and /^\), you need to write an instance for Primitive.

class Primitive a b where
  mkPrim :: a -> b

mkTuple :: forall a b c. Primitive b c => a -> b -> Tuple a c
mkTuple key value = Tuple key (mkPrim value)

infixl 6 mkTuple as /^\
