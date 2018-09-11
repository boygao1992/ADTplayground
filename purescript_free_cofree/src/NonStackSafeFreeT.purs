module NonStackSafeFreeT where

import Prelude

import Control.Monad (class Monad, pure)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Functor (class Functor)

newtype FreeT f m a = FreeT (m (Either a (f (FreeT f m a))))

resumeT :: forall f m a. FreeT f m a -> m (Either a (f (FreeT f m a)))
resumeT (FreeT a) = a

instance functorFreeT :: (Functor f, Functor m) => Functor (FreeT f m) where
  map f = FreeT <<< map (bimap f (map (map f))) <<< resumeT

instance applyFreeT :: (Functor f, Monad m) => Apply (FreeT f m) where
  apply = ap

instance applicativeFreeT :: (Functor f, Monad m) => Applicative (FreeT f m) where
  pure = FreeT <<< pure <<< Left

instance bindFreeT :: (Functor f, Monad m) => Bind (FreeT f m) where
  bind (FreeT a) f = FreeT (a >>= go)
    where
      go (Left next) = resumeT (f next)
      go (Right fs) = pure $ Right $ map (_ >>= f) fs

instance monadFreeT :: (Functor f, Monad m) => Monad (FreeT f m)

instance moandTransFreeT :: Functor f => MonadTrans (FreeT f) where
  lift = FreeT <<< map Left

liftFreeT :: forall f m a. Functor f => Monad m => f a -> FreeT f m a
liftFreeT = FreeT <<< pure <<< Right <<< map pure

runFreeT
  :: forall f m a
   . Monad m
   =>(f (FreeT f m a) -> m (FreeT f m a))
   -> FreeT f m a
   -> m a
runFreeT phi =
  either
    pure
    ((_ >>= (runFreeT phi)) <<< phi) <=< resumeT
