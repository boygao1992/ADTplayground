module Data.Vector where

import Prelude

import Data.Maybe (Maybe (..), fromJust)
import Type.Data.Symbol (SProxy (..))
import Data.Array as Array
import Data.Newtype (class Newtype)
import Partial.Unsafe (unsafePartial)
import Type.Data.Symbol.Num.Int as Int
import Type.Data.Symbol.Num.Nat as Nat

newtype Vector (dim :: Symbol) a = Vector (Array a)
derive instance newtypeVector :: Newtype (Vector dim a) _
instance showVector :: (Nat.IsNat dim, Show a) => Show (Vector dim a) where
  show (Vector v) = show v

nil :: forall a. Vector "0" a
nil = Vector []

cons
  :: forall a dim1 dim2
   . Nat.Add dim1 "1" dim2
   => a -> Vector dim1 a -> Vector dim2 a
cons a (Vector v1) = Vector (a `Array.cons` v1)

infixr 5 cons as +>

uncons
  :: forall a dim1 dim2
   . Int.Pred dim1 dim2
   => Nat.IsNat dim2
   => Vector dim1 a
   -> { head :: a, tail :: Vector dim2 a }
uncons (Vector v) =
  let
    { head: h, tail: t } = unsafePartial $ fromJust $ Array.uncons v
  in
    { head: h, tail: Vector t }

singleton :: forall a. a -> Vector "1" a
singleton a = Vector [a]

vec2 :: forall a. a -> a -> Vector "2" a
vec2 x y = Vector [x, y]

vec3 :: forall a. a -> a -> a -> Vector "3" a
vec3 x y z = Vector [x, y, z]

fill :: forall dim a. Nat.IsNat dim => (Int -> a) -> Vector dim a
fill f = Vector
         <<< map f
         <<< Array.dropEnd 1 <<< Array.range 0 $ Nat.toInt (SProxy :: SProxy dim)

range' :: forall dim.  Nat.IsNat dim => Int -> Vector dim Int
range' i = fill (_ + i)

range :: forall dim. Nat.IsNat dim => Int -> SProxy dim -> Vector dim Int
range i _ = range' i

replicate' :: forall dim a. Nat.IsNat dim => a -> Vector dim a
replicate' = Vector <<< Array.replicate (Nat.toInt (SProxy :: SProxy dim))

replicate :: forall dim a. Nat.IsNat dim => SProxy dim -> a -> Vector dim a
replicate _ = replicate'

fromArray :: forall dim a. Nat.IsNat dim => Array a -> Maybe (Vector dim a)
fromArray xs =
  if Array.length xs == Nat.toInt (SProxy :: SProxy dim)
  then Just $ Vector xs
  else Nothing

length :: forall dim a. Nat.IsNat dim => Vector dim a -> Int
length _ = Nat.toInt (SProxy :: SProxy dim)

lengthT :: forall dim a. Nat.IsNat dim => Vector dim a -> SProxy dim
lengthT _ = SProxy :: SProxy dim

toArray :: forall dim a. Nat.IsNat dim => Vector dim a -> Array a
toArray (Vector v) = v

