module Selda.Selectors where

import Prelude

import Data.Array ((!!), (:))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Exists (Exists, flippedRunExists, mkExists)
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Unsafe.Coerce (unsafeCoerce)

import Selda.Column (Col(..), Row(..))
import Selda.Exp (SomeExp, UntypedCol(..), runSomeExp, untyped)
import Selda.SQL (SQL)
import Selda.SqlRow (class SqlRow)
import Selda.SqlType (class SqlType)

-- | A column selector. Column selectors can be used together with the '!' and
--   'with' functions to get and set values on rows, or to specify
--   foreign keys.
newtype Selector t a = Selector Int

-- | Coalesce nested nullable column into a single level of nesting.
-- type family Coalesce a where
--   Coalesce (Maybe (Maybe a)) = Coalesce (Maybe a)
--   Coalesce a                 = a
class Coalesce i o | i -> o
instance coalesceInductionStep
  :: Coalesce (Maybe a) o => Coalesce (Maybe (Maybe a)) o
else
instance coalesceBaseCase :: Coalesce a a


-- | A selector indicating the nth (zero-based) column of a table.
--
--   Will cause errors in queries during compilation, execution, or both,
--   unless handled with extreme care. You really shouldn't use it at all.
unsafeSelector :: forall a b. SqlRow a => SqlType b => Int -> Selector a b
unsafeSelector = Selector

-- | Extract the given column from the given row.
extractColumn :: forall s t a. SqlType a => Row s t -> Selector t a -> Col s a
extractColumn (Many xs) (Selector i) = case xs !! i of
  Nothing -> unsafeThrow "invalid Selector when extractColumn" -- NOTE unsafe
  Just (Untyped x) -> One (unsafeCoerce x)
infixl 9 extractColumn as !

-- | Extract the given column from the given nullable row.
--   Nullable rows usually result from left joins.
--   If a nullable column is extracted from a nullable row, the resulting
--   nested @Maybe@s will be squashed into a single level of nesting.
-- NOTE (?) :: Partial => SqlType a => Row s (Maybe t) -> Selector t a -> Col s (Coalesce (Maybe a))
extractNullableColumn
  :: forall s t a o
  . SqlType a
  => Coalesce (Maybe a) o
  => Row s (Maybe t) -> Selector t a -> Col s o
extractNullableColumn (Many xs) (Selector i) = case xs !! i of
  Nothing -> unsafeThrow "invalid Selector when extractNullableColumn" -- NOTE unsafe
  Just (Untyped x) -> One (unsafeCoerce x)
infixl 9 extractNullableColumn as ?

-- TODO optimize implementation
splitAt :: forall a. Int -> Array a -> Array a /\ Array a
splitAt i xs
  = bimap Array.fromFoldable Array.fromFoldable
  $ Seq.splitAt i
  $ Seq.fromFoldable xs

-- updateColumn
upd :: forall s t. Row s t -> Assignment s t -> Row s t
upd (Many xs) (Assign assignF)
  = flippedRunExists assignF \(AssignF (Selector i) (One x')) ->
  let ( left /\ after ) = splitAt i xs
  in case Array.tail after of
    Nothing -> unsafeThrow "BUG: too few columns in row!"
    Just right -> Many (left <> untyped x' : right)

upd (Many (xs :: Array (UntypedCol SQL))) (Modify modifyF)
  = flippedRunExists modifyF \(ModifyF (Selector i) f) ->
  let ( left /\ after ) = splitAt i xs
  in case Array.uncons after of
    Nothing -> unsafeThrow "BUG: too few columns in row!"
    Just { head: Untyped (x :: SomeExp SQL)
         , tail: right } ->
      let
        y' = runSomeExp x \eX ->
          -- NOTE unsafeCoerce
          -- f :: exists a1. Col s a1 -> Col s a1
          -- x :: exists a2. Exp SQL a2
          -- s (in Row s t) ~ s (in Assignment s t)
          -- => a1 ~ a2
          -- FIXME not sure the extra phantom type in GADT/ADT is of any help if manual proof injection is needed. will see
          case f (One (unsafeCoerce eX)) of
            One y -> untyped y
      in
        Many (left <> y' : right)

-- | A selector-value assignment pair.
{-
data Assignment s t where
  -- | Set the given column to the given value.
  (:=) :: forall a. Selector t a -> Col s a -> Assignment s t
  -- | Modify the given column by the given function.
  Modify :: forall a. Selector t a -> (Col s a -> Col s a) -> Assignment s t
infixl 2 :=
-}

data Assignment s t
  = Assign (Exists (AssignF s t))
  | Modify (Exists (ModifyF s t))
data AssignF s t a = AssignF (Selector t a) (Col s a)
data ModifyF s t a = ModifyF (Selector t a) (Col s a -> Col s a)
assign :: forall s t a. Selector t a -> Col s a -> Assignment s t
assign s c = Assign $ mkExists $ AssignF s c
modify :: forall s t a. Selector t a -> (Col s a -> Col s a) -> Assignment s t
modify s f = Modify $ mkExists $ ModifyF s f
infixl 2 assign as :=
infixl 2 modify as $=

-- | For each selector-value pair in the given list, on the given tuple,
--   update the field pointed out by the selector with the corresponding value.
with :: forall s a. Row s a -> Array (Assignment s a) -> Row s a
with = Array.foldl upd
