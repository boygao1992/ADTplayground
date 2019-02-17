module Data.ApplyAlgebra where

import Prelude

import Control.Apply (lift2)
import Data.Newtype (class Newtype)


newtype ApplyAlgebra (f :: Type -> Type) a = ApplyAlgebra (f a)
derive instance newtypeApplyAlgebra :: Newtype (ApplyAlgebra f a) _

applyAlgebraLift :: forall f a b. (f a -> f b) -> ApplyAlgebra f a -> ApplyAlgebra f b
applyAlgebraLift f (ApplyAlgebra a) = ApplyAlgebra (f a)

applyAlgebraLift2 :: forall f a b c. (f a -> f b -> f c) -> ApplyAlgebra f a -> ApplyAlgebra f b -> ApplyAlgebra f c
applyAlgebraLift2 f (ApplyAlgebra a) (ApplyAlgebra b) = ApplyAlgebra (f a b)

instance semigroupApplyAlgebra ::
  ( Apply f
  , Semigroup a
  ) => Semigroup (ApplyAlgebra f a)
  where
    append = applyAlgebraLift2 (lift2 append)

instance monoidApplyAlgebra ::
  ( Applicative f
  , Monoid a
  ) => Monoid (ApplyAlgebra f a)
  where
    mempty = ApplyAlgebra (pure mempty)
