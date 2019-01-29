{- NOTE
adopted from
[rightfold / purescript-nullable-safe](https://github.com/rightfold/purescript-nullable-safe)
-}

module Data.Nullable.Safe
  ( Nullable
  , class NeverNull

  , null
  , nonNull
  , nullable
  , neverNull

  , mapNullable, (?<$>)
  , applyNullable, (?<*>)
  , pureNullable
  , bindNullable, (?>>=)
  , bindNullableFlipped, (?=<<)
  , composeKleisliNullable, (?>=>)
  , composeKleisliNullableFlipped, (?<=<)

  , fromMaybe
  , toMaybe
  ) where

import Prelude

import Data.Function (on)
import Data.Maybe (Maybe (..), maybe)
import Prim.TypeError (class Fail, Text)
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, runFn0, runFn1, runFn2, runFn3)



-- | `Nullable a` is either `a` or `null`. The runtime representation is either
-- | that of `a` or `null`.
foreign import data Nullable :: Type -> Type

instance eqNullable :: (Eq a) => Eq (Nullable a) where
  eq = eq `on` toMaybe

instance ordNullable :: (Ord a) => Ord (Nullable a) where
  compare = compare `on` toMaybe

instance showNullable :: (Show a) => Show (Nullable a) where
  show = nullable "null" (\x -> "(just " <> show x <> ")")



-- | Do not instantiate this class unless your type has a runtime representation
-- | that is never `null`.
class NeverNull a

-- NOTE experiment
instance neverNullNullable ::
  Fail
  ( Text "Expect something other than Nullable.")
  => NeverNull (Nullable a)
else instance neverNullOther ::
  NeverNull a

-- instance neverNullVoid :: NeverNull Void
-- instance neverNullBoolean :: NeverNull Boolean
-- instance neverNullChar :: NeverNull Char
-- instance neverNullInt :: NeverNull Int
-- instance neverNullNumber :: NeverNull Number
-- instance neverNullString :: NeverNull String
-- instance neverNullArray :: NeverNull (Array a)
-- instance neverNullRecord :: NeverNull {| r}
-- instance neverNullFunction :: NeverNull (a -> b)
-- instance neverNullOrdering :: NeverNull Ordering
-- instance neverNullMaybe :: NeverNull (Maybe a)


-- | `null`.
null :: ∀ a. NeverNull a => Nullable a
null = runFn0 _null

-- | `just`.
-- NOTE renamed to `nonNull`
nonNull :: ∀ a. NeverNull a => a -> Nullable a
nonNull a = runFn1 _nonNull a

-- | Fold a `Nullable`.
nullable :: ∀ a b. b -> (a -> b) -> Nullable a -> b
nullable b f a = runFn3 _nullable b f a

-- | Recover a `NeverNull` evidence from a `Nullable`. The first argument is
-- | ignored.
neverNull :: ∀ a b. Nullable a -> (NeverNull a => b) -> b
neverNull x f = runFn2 _neverNull x f



-- | Change the value inside a `Nullable` using a function.
mapNullable :: ∀ a b. NeverNull b => (a -> b) -> Nullable a -> Nullable b
mapNullable f = nullable null (nonNull <<< f)
infixl 4 mapNullable as ?<$>

-- | Change the value inside a `Nullable` using a function in another
-- | `Nullable`.
applyNullable :: ∀ a b. NeverNull b => Nullable (a -> b) -> Nullable a -> Nullable b
applyNullable f x = nullable null (\f' -> nullable null (nonNull <<< f') x) f
infixl 4 applyNullable as ?<*>

-- | `just`.
pureNullable :: ∀ a. NeverNull a => a -> Nullable a
pureNullable = nonNull

-- | Change the value inside a `Nullable` using a function and flatten.
bindNullable :: ∀ a b. NeverNull b => Nullable a -> (a -> Nullable b) -> Nullable b
bindNullable x k = nullable null k x
infixl 1 bindNullable as ?>>=

bindNullableFlipped :: ∀ a b. NeverNull b => (a -> Nullable b) -> Nullable a -> Nullable b
bindNullableFlipped = flip bindNullable
infixr 1 bindNullableFlipped as ?=<<

-- | Compositional form of `(?>>=)`.
composeKleisliNullable :: ∀ a b c. NeverNull c => (a -> Nullable b) -> (b -> Nullable c) -> a -> Nullable c
composeKleisliNullable k l x = k x ?>>= l
infixr 1 composeKleisliNullable as ?>=>

composeKleisliNullableFlipped :: ∀ a b c. NeverNull c => (b -> Nullable c) -> (a -> Nullable b) -> a -> Nullable c
composeKleisliNullableFlipped = flip composeKleisliNullable
infixr 1 composeKleisliNullableFlipped as ?<=<



-- | Convert a `Maybe` to a `Nullable`.
fromMaybe :: ∀ a. NeverNull a => Maybe a -> Nullable a
fromMaybe = maybe null nonNull

-- | Convert a `Nullable` to a `Maybe`.
toMaybe :: ∀ a. Nullable a -> Maybe a
toMaybe = nullable Nothing Just


foreign import _null :: forall a. NeverNull a => Fn0 (Nullable a)
foreign import _nonNull :: forall a. NeverNull a => Fn1 a (Nullable a)
foreign import _nullable :: forall a b. Fn3 b (a -> b) (Nullable a) b
foreign import _neverNull :: forall a b. Fn2 (Nullable a) (NeverNull a => b) b
