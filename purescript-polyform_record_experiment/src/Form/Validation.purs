module Form.Validation where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Control.Plus (class Plus, empty)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, under)
import Data.Profunctor (class Profunctor)
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

_Valid :: forall r a. Prism' (V r a) (Tuple r a)
_Valid =
  prism'
    (\(Tuple r a) -> Valid r a)
    case _ of
      Valid r a -> Just $ Tuple r a
      _ -> Nothing

_Invalid :: forall r a. Prism' (V r a) r
_Invalid = _Ctor' (SProxy :: SProxy "Invalid")

fromEither :: forall r a. Monoid r => Either r a -> V r a
fromEither (Left r) = Invalid r
fromEither (Right a) = Valid mempty a

toEither :: forall r a. V r a -> Either r a
toEither (Invalid r) = Left r
toEither (Valid _ a) = Right a

toMaybe :: forall r a. V r a -> Maybe a
toMaybe (Valid _ a) = Just a
toMaybe _ = Nothing

-- | Validation

newtype Validation m r i o = Validation (i -> m (V r o))
derive instance newtypeValidation :: Newtype (Validation m r i o) _
derive instance functorValidation :: Functor m => Functor (Validation m r i)

runValidation :: forall m r i o. Validation m r i o -> i -> m (V r o)
runValidation = unwrap

instance applyValidation
  :: (Monad m, Semigroup r) => Apply (Validation m r i)
  where
    apply (Validation k1) (Validation k2)
      = Validation \i -> do
          v1 <- k1 i
          v2 <- k2 i
          pure $ v1 <*> v2

instance applicativeValidation
  :: (Monad m, Monoid r) => Applicative (Validation m r i)
  where
    pure = Validation <<< const <<< pure <<< pure

instance bindValidation
  :: (Monad m, Monoid r) => Bind (Validation m r i)
  where
    bind v1 k = Validation \i ->
      runValidation v1 i >>= case _ of
        Invalid r1 -> pure $ Invalid r1
        Valid r1 x ->
          runValidation (k x) i >>= case _ of
            Invalid r2 -> pure $ Invalid (r1 <> r2)
            Valid r2 y -> pure $ Valid (r1 <> r2) y

instance monadValidation :: (Monad m, Monoid r) => Monad (Validation m r i)

instance altValidation :: (Monad m, Semigroup r) => Alt (Validation m r i) where
  alt (Validation l1) (Validation l2) = Validation \i -> do
    v1 <- l1 i
    v2 <- l2 i
    pure $ v1 <|> v2

instance plusValidation :: (Monad m, Monoid r) => Plus (Validation m r i) where
  empty = Validation <<< const <<< pure $ empty

instance profunctorValidation :: Functor m => Profunctor (Validation m r) where
  dimap f g (Validation v) = Validation $ map (map g) <<< v <<< f

instance semigroupoidValidation
  :: (Monad m, Monoid r) => Semigroupoid (Validation m r)
  where
    compose (Validation l2) (Validation l1) = Validation \a -> do
      v1 <- l1 a
      case v1 of
        Invalid r1 -> pure $ Invalid r1
        Valid r1 b -> do
          v2 <- l2 b
          case v2 of
            Invalid r2 -> pure $ Invalid (r1 <> r2)
            Valid r2 c -> pure $ Valid (r1 <> r2) c

instance categoryValidation
  :: (Monad m, Monoid r) => Category (Validation m r)
  where
    identity = Validation $ pure <<< pure

-- | BifunctorValidation
newtype BifunctorValidation m i r o = BifunctorValidation (Validation m r i o)
derive instance newtypeBifunctorValidation :: Newtype (BifunctorValidation m i r o) _

instance bifunctorBifunctorValidation
  :: Monad m => Bifunctor (BifunctorValidation m i)
  where
    bimap f g (BifunctorValidation (Validation l))
      = BifunctorValidation
        $ Validation \i -> do
          v <- l i
          pure $ bimap f g v

bimapValidation
  :: forall m r1 r2 i o1 o2
  .  Monad m
  => (r1 -> r2)
  -> (o1 -> o2)
  -> Validation m r1 i o1
  -> Validation m r2 i o2
bimapValidation l r = under BifunctorValidation (bimap l r)

lmapValidation
  :: forall m r1 r2 i o
  .  Monad m
  => (r1 -> r2)
  -> Validation m r1 i o
  -> Validation m r2 i o
lmapValidation l = under BifunctorValidation (lmap l)

rmapValidation
  :: forall m r i o1 o2
  .  Monad m
  => (o1 -> o2)
  -> Validation m r i o1
  -> Validation m r i o2
rmapValidation r = under BifunctorValidation (rmap r)

-- | API

hoistFn :: forall m r i o. Monad m => Monoid r => (i -> o) -> Validation m r i o
hoistFn f = Validation $ pure <<< pure <<< f

hoistFnV :: forall m r i o. Monad m => Monoid r => (i -> V r o) -> Validation m r i o
hoistFnV k = Validation $ pure <<< k

hoistFnMV :: forall m r i o. Monad m => Monoid r => (i -> m (V r o)) -> Validation m r i o
hoistFnMV = Validation
