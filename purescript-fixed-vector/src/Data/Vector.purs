module Data.Vector where

import Prelude

import Data.Array as Array
import Data.Traversable (class Foldable, class Traversable, sum)
import Control.Apply (lift2)
import Num.Nat as Nat
import Partial.Unsafe (unsafePartial)
import Type.Data.Ordering as Ord
import Type.Data.Symbol as Symbol
import Type.Prelude (SProxy(..))

newtype Vec (n :: Symbol) a = Vec (Array a)
derive newtype instance functorVec :: Functor (Vec n)
derive newtype instance applyVec :: Apply (Vec n)
derive newtype instance applicativeVec :: Applicative (Vec n)
derive newtype instance eqVec :: Eq a => Eq (Vec n a)
derive newtype instance ordVec :: Ord a => Ord (Vec n a)
derive newtype instance foldableVec :: Foldable (Vec n)
derive newtype instance traversableVec :: Traversable (Vec n)
instance showVec :: (Symbol.IsSymbol n, Show a) => Show (Vec n a) where
  show (Vec arr)
    = "V" <> Symbol.reflectSymbol (SProxy :: SProxy n) <> " " <> show arr
instance semiringVec :: (Nat.IsNat n, Semiring a) => Semiring (Vec n a) where
  add = lift2 (+)
  zero = replicate zero
  mul = lift2 (*)
  one = replicate one
instance ringVec :: (Nat.IsNat n, Ring a) => Ring (Vec n a) where
  sub = lift2 (-)

type Zero = "0"

nil :: forall a. Vec Zero a
nil = Vec []

cons :: forall n n1 a. Nat.IsNat n => Nat.Succ n n1 => a -> Vec n a -> Vec n1 a
cons a (Vec arr)= Vec $ Array.cons a arr
infixr 6 cons as :|

replicate :: forall n a. Nat.IsNat n => a -> Vec n a
replicate a = Vec $ Array.replicate (Nat.reflectNat (SProxy :: SProxy n)) a

vec1 :: forall a. a -> Vec "1" a
vec1 x = x :| nil

vec2 :: forall a. a -> a -> Vec "2" a
vec2 x y = x :| y :| nil

vec3 :: forall a. a -> a -> a -> Vec "3" a
vec3 x y z = x :| y :| z :| nil

index
  :: forall n m a
  . Nat.IsNat n
  => Nat.IsNat m
  => Nat.Compare n m Ord.GT
  => Vec n a -> SProxy m -> a
index (Vec arr) idx = unsafePartial $ Array.unsafeIndex arr $ Nat.reflectNat idx
infixl 6 index as |!!

scale :: forall n a. Semiring a => a -> Vec n a -> Vec n a
scale a (Vec arr) = Vec $ (a * _) <$> arr
infixl 6 scale as *|

dot :: forall n a. Nat.IsNat n => Semiring a => Vec n a -> Vec n a -> a
dot v1 v2 = sum $ v1 * v2
infixl 6 dot as |.|
