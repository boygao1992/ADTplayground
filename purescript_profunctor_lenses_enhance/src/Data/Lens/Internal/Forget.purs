module Data.Lens.Internal.Forget where

import Prelude

import Data.Profunctor (class Profunctor)


newtype Forget r a b = Forget (a -> r)
instance profunctorForget :: Profunctor (Forget r) where
  dimap :: forall s t a b. (s -> a) -> (b -> t) -> Forget r a b -> Forget r s t
  -- dimap pre post (Forget p) = Forget $ dimap pre identity p
  dimap pre post (Forget f) = Forget (pre >>> f)
