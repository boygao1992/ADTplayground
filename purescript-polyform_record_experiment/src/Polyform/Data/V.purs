module Polyform.Data.V where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Generic.Optic (_Ctor')
import Type.Data.Symbol (SProxy(..))

-- | V

data V r a = Invalid r | Valid r a

derive instance genericV :: Generic (V r a) _

instance showV ∷ (Show e, Show a) => Show (V e a) where
  show (Invalid e) = "(Invalid " <> show e <> ")"
  show (Valid e a) = "(Valid " <> show e <> " " <> show a <> ")"

derive instance functorV :: Functor (V r)

instance applyV :: Semigroup r => Apply (V r) where
  apply (Invalid r1) (Invalid r2) = Invalid (r1 <> r2)
  apply (Invalid r1) (Valid r2 _) = Invalid (r1 <> r2)
  apply (Valid r1 _) (Invalid r2) = Invalid (r1 <> r2)
  apply (Valid r1 f) (Valid r2 x) = Valid (r1 <> r2) (f x)

instance applicativeV :: Monoid r => Applicative (V r) where
  pure = Valid mempty

instance bindV :: Semigroup r => Bind (V r) where
  bind (Valid r1 x) k = case k x of
    Invalid r2 -> Invalid (r1 <> r2)
    Valid r2 y -> Valid (r1 <> r2) y
  bind (Invalid r) _ = Invalid r

instance monadV :: Monoid r => Monad (V r)

instance altV :: Semigroup r => Alt (V r) where
  alt (Valid r1 x) (Valid r2 _) = Valid (r1 <> r2) x
  alt (Valid r1 x) (Invalid r2) = Valid (r1 <> r2) x
  alt (Invalid r1) (Valid r2 y) = Valid (r1 <> r2) y
  alt (Invalid r1) (Invalid r2) = Invalid (r1 <> r2)

instance plusV :: Monoid r => Plus (V r) where
  empty = Invalid mempty

instance bifunctorV :: Bifunctor V where
  bimap f _ (Invalid r) = Invalid $ f r
  bimap f g (Valid r a) = Valid (f r) (g a)

-- NOTE not Alternative, Annihilation law doesn't hold

instance semigroupV :: (Semigroup r, Semigroup a) => Semigroup (V r a) where
  append = lift2 append
instance monoidV :: (Monoid r, Monoid a) => Monoid (V r a) where
  mempty = pure mempty

instance foldableV :: Foldable (V r) where
  foldMap f (Invalid _) = mempty
  foldMap f (Valid _ a) = f a

  foldr f b (Invalid _) = b
  foldr f b (Valid _ a) = f a b

  foldl f b (Invalid _) = b
  foldl f b (Valid _ a) = f b a

instance traversableV :: Traversable (V r) where
  sequence (Invalid r) = pure (Invalid r)
  sequence (Valid r a) = Valid r <$> a

  traverse _ (Invalid r) = pure (Invalid r)
  traverse f (Valid r a) = Valid r <$> f a

_Valid :: forall r a. Prism' (V r a) (Tuple r a)
_Valid =
  prism'
    (\(Tuple r a) -> Valid r a)
    case _ of
      Valid r a -> Just $ Tuple r a
      _ -> Nothing

_Invalid :: forall r a. Prism' (V r a) r
_Invalid = _Ctor' (SProxy :: SProxy "Invalid")

fromEither :: forall r a. Monoid r => r -> Either r a -> V r a
fromEither _ (Left r) = Invalid r
fromEither r (Right a) = Valid r a

fromEither' :: forall r a. Monoid r => Either r a -> V r a
fromEither' (Left r) = Invalid r
fromEither' (Right a) = Valid mempty a

fromMaybe :: forall r a. Monoid r => r -> r -> Maybe a -> V r a
fromMaybe r _ Nothing = Invalid r
fromMaybe _ r (Just a) = Valid r a

fromMaybe' :: forall r a. Monoid r => Maybe a -> V r a
fromMaybe' Nothing = Invalid mempty
fromMaybe' (Just a) = Valid mempty a

toEither :: forall r a. V r a -> Either r a
toEither (Invalid r) = Left r
toEither (Valid _ a) = Right a

toMaybe :: forall r a. V r a -> Maybe a
toMaybe (Valid _ a) = Just a
toMaybe _ = Nothing
