module Polyform.Validation where

import Prelude

import Control.Alt (class Alt, (<|>), alt)
import Control.Parallel.Class (class Parallel)
import Control.Parallel.Class (parallel, sequential) as Par
import Control.Plus (class Plus, empty)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Newtype (class Newtype, unwrap, under, wrap)
import Data.Profunctor (class Profunctor)
import Polyform.Data.V (V(..))

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

-- instance bindValidation
--   :: (Monad m, Monoid r) => Bind (Validation m r i)
--   where
--     bind v1 k = Validation \i ->
--       runValidation v1 i >>= case _ of
--         Invalid r1 -> pure $ Invalid r1
--         Valid r1 x ->
--           runValidation (k x) i >>= case _ of
--             Invalid r2 -> pure $ Invalid (r1 <> r2)
--             Valid r2 y -> pure $ Valid (r1 <> r2) y

-- instance monadValidation :: (Monad m, Monoid r) => Monad (Validation m r i)

instance altValidation :: (Monad m, Semigroup r) => Alt (Validation m r i) where
  alt (Validation l1) (Validation l2) = Validation \i -> do
    v1 <- l1 i
    v2 <- l2 i
    pure $ v1 <|> v2

instance plusValidation :: (Monad m, Monoid r) => Plus (Validation m r i) where
  empty = Validation <<< const <<< pure $ empty

{- NOTE
newtype Star f a b = Star (a -> f b)
instance Functor f => Profunctor (Star f)

ValidationT i r m o
---------------------------
i -> ValidationResultT r m o
---------------------------
Star (ValidationResultT r m)

instance Functor m => Profunctor (Star (ValidationResultT r m))
-}
instance profunctorValidation :: Functor m => Profunctor (Validation m r) where
  dimap f g (Validation v) = Validation $ map (map g) <<< v <<< f

{- NOTE
instance Bind m => Semigroupoid (Star (ValidationResultT r m))
-}
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

{- NOTE
instance Monad m => Category (Star (ValidationResultT r m))
-}
instance categoryValidation
  :: (Monad m, Monoid r) => Category (Validation m r)
  where
    identity = Validation $ pure <<< pure

hoistFn :: forall m r i o. Monad m => Monoid r => (i -> o) -> Validation m r i o
hoistFn f = Validation $ pure <<< pure <<< f

hoistFnV
  :: forall m r i o. Monad m => Monoid r => (i -> V r o) -> Validation m r i o
hoistFnV k = Validation $ pure <<< k

hoistFnMV
  :: forall m r i o. Monad m => Monoid r => (i -> m (V r o)) -> Validation m r i o
hoistFnMV = Validation

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

-- | ParValidation

newtype ParValidation m r i o = ParValidation (Validation m r i o)
derive instance newtypeParValidation :: Newtype (ParValidation m r i o) _
derive instance functorParValidation :: Functor m => Functor (ParValidation m r i)

instance applyParValidation
  :: (Monad m, Parallel f m, Semigroup r) => Apply (ParValidation m r i)
  where
    apply (ParValidation (Validation vf)) (ParValidation (Validation va))
      = ParValidation <<< Validation $ \i ->
      Par.sequential $ apply <$> Par.parallel (vf i) <*> Par.parallel (va i)

instance applicativeParValidation
  :: (Monad m, Parallel f m, Monoid r) => Applicative (ParValidation m r i)
  where
    pure = ParValidation <<< pure

instance altParValidation
  :: (Monad m, Parallel f m, Semigroup r) => Alt (ParValidation m r i)
  where
    alt (ParValidation (Validation mv1)) (ParValidation (Validation mv2))
      = ParValidation <<< Validation $ \i ->
          Par.sequential $ alt <$> Par.parallel (mv1 i) <*> Par.parallel (mv2 i)

instance plusParValidation
  :: (Monad m, Parallel f m, Monoid r) => Plus (ParValidation m r i)
  where
    empty = ParValidation empty

-- instance parallelParValidation
--   :: (Monad m, Parallel f m, Monoid r)
--   => Parallel (ParValidation m r i) (Validation m r i)
--   where
--     parallel = wrap
--     sequential = unwrap


