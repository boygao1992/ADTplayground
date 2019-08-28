module MonadVar.Classes where

import Prelude

import Data.Tuple.Nested (type (/\))

class Monad m <= MonadNew m v where
  new :: forall a. a -> m (v a)

class Monad m <= MonadRead m v where
  read :: forall a. v a -> m a

class Monad m <= MonadWrite m v where
  write :: forall a. v a -> a -> m Unit

class Monad m <= MonadSwap m v where
  swap :: forall a. v a -> a -> m a

class MonadWrite m v <= MonadMutate_ m v where
  mutate_ :: forall a. v a -> (a -> a) -> m Unit

class (MonadRead m v, MonadMutate_ m v) <= MonadMutate m v where
  mutate :: forall a b. v a -> (a -> (a /\ b)) -> m b

class MonadMutate_ m v <= MonadMutateM_ f m v where
  mutateM_ :: forall a. v a -> (a -> f a) -> m Unit

class (MonadMutate m v, MonadMutateM_ f m v) <= MonadMutateM f m v where
  mutateM :: forall a b. v a -> (a -> f (a /\ b)) -> m b

-- TODO class MonadRead m v <= MonadLock m v where
-- TODO class Monad m <= MonadFoldMutate m v where
-- TODO Monad m <= MonadFoldMutateM m n v where
