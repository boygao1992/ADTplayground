module Selda.Nullable where

import Prelude

import Type.Equality (class TypeEquals)
import Type.Equality as Type

import Data.Array as Array
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)

import Selda (class SqlOrd, isNull, not_, (./=), (.<), (.<=), (.==), (.>), (.>=))
import Selda.Query (restrict)
import Selda.Query.Type (Query)
import Selda.Exp (UntypedCol(..))
import Selda.Column (class Same, Col(..), Row(..))
import Selda.Column (from, to) as Same
import Selda.Selectors (class Coalesce, Selector(..))
import Selda.SqlType (class SqlType)
import Selda.Unsafe (cast)

class NonNull i o | i -> o

instance nonNullMaybe :: NonNull (Maybe i) i
else
instance nonNullOtherwise :: NonNull i i

-- | Unconditionally convert a nullable value into a non-nullable one,
--   using the standard SQL null-coalescing behavior.
fromNullable
  :: forall s a a'
  . NonNull a a'
  => SqlType a'
  => Col s a
  -> Col s a'
fromNullable = unsafeCoerce

nullableEq
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => SqlType a
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableEq a b = cast $ fromNullable a .== Type.from (fromNullable b :: Col t b')
infixl 4 nullableEq as ?==

nullableNotEq
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => SqlType a
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableNotEq a b = cast $ fromNullable a ./= Type.from (fromNullable b :: Col t b')
infixl 4 nullableNotEq as ?/=

nullableGt
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => SqlOrd a'
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableGt a b = cast $ fromNullable a .> Type.from (fromNullable b :: Col t b')
infixl 4 nullableGt as ?>

nullableLt
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => SqlOrd a'
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableLt a b = cast $ fromNullable a .< Type.from (fromNullable b :: Col t b')
infixl 4 nullableLt as ?<

nullableGeq
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => SqlOrd a'
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableGeq a b = cast $ fromNullable a .>= Type.from (fromNullable b :: Col t b')
infixl 4 nullableGeq as ?>=

nullableLeq
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => SqlOrd a'
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableLeq a b = cast $ fromNullable a .<= Type.from (fromNullable b :: Col t b')
infixl 4 nullableLeq as ?<=

nullablePlus
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => Semiring a'
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullablePlus a b =
  cast $ fromNullable a + Same.from (Type.from (fromNullable b :: Col t b'))
infixl 4 nullablePlus as ?+

nullableMinus
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => Ring a'
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableMinus a b =
  cast $ fromNullable a - Same.from (Type.from (fromNullable b :: Col t b'))
infixl 4 nullableMinus as ?-

nullableMultiply
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => Semiring a'
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableMultiply a b =
  cast $ fromNullable a * Same.from (Type.from (fromNullable b :: Col t b'))
infixl 4 nullableMultiply as ?*

nullableDivide
  :: forall s t a b a' b'
  . NonNull a a'
  => NonNull b b'
  => TypeEquals (Col t a') (Col t b')
  => SqlType a'
  => SqlType b'
  => EuclideanRing a'
  => Same s t
  => Col s a
  -> Col t b
  -> Col s (Maybe Boolean)
nullableDivide a b =
  cast $ fromNullable a / Same.from (Type.from (fromNullable b :: Col t b'))
infixl 4 nullableDivide as ?/

nullableIndex
  :: forall s t a t' ma
  . SqlType a
  => NonNull t t'
  => Coalesce (Maybe a) ma
  => Row s t
  -> Selector t' a
  -> Col s ma
nullableIndex (Many xs) (Selector i) = case xs `unsafeIndex` i of
    Untyped x -> One (unsafeCoerce x)
  where
    unsafeIndex = unsafePartial Array.unsafeIndex

nonNull :: forall s t a. Same s t => SqlType a => Col s (Maybe a) -> Query t (Col t a)
nonNull x = do
  restrict (not_ (isNull x))
  pure (fromNullable (Same.to x))

-- | Restrict a query using a nullable expression.
--   Equivalent to @restrict . ifNull false@.
restrict' :: forall s t. Same s t => Col s (Maybe Boolean) -> Query t Unit
restrict' = restrict <<< fromNullable
