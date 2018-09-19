module OrdBox where

import Prelude

-- | a data-type that carries Ord (inherits Eq) constraint
data OrdBox a = OrdBox (a -> a -> Boolean) (a -> a -> Ordering) a

instance eqOrdBox :: Eq (OrdBox a) where
  eq :: OrdBox a -> OrdBox a -> Boolean
  eq (OrdBox e _ x) (OrdBox _ _ y) = e x y

instance ordOrdBox :: Ord (OrdBox a) where
  compare :: OrdBox a -> OrdBox a -> Ordering
  compare (OrdBox _ c x) (OrdBox _ _ y) = c x y

-- | a constructor that verifies the Ord constraint on data-type a
mkOrdBox :: forall a. Ord a => a -> OrdBox a
mkOrdBox = OrdBox eq compare -- grab the implementation of `eq` and `compare` out of `a`'s dictionary

-- | the Ord constraint is no longer carried
unOrdBox :: forall a. OrdBox a -> a
unOrdBox (OrdBox _ _ a) = a
