module Free where

import Prelude

data Free m a
  = Roll (m (Free m a))
  | Pure a

instance functorFree :: Functor m => Functor (Free m) where
  -- map :: forall a b. (a -> b) -> m a -> m b
  map g (Pure a) = Pure (g a)
  map g (Roll mfa) = Roll (map (map g) mfa)

instance applyFree :: Functor m => Apply (Free m) where
  -- apply :: forall a b. f(a -> b) -> m a -> m b
  apply (Pure g) (Pure a) = Pure (g a)
  apply (Pure g) (Roll mfa) = Roll (map (map g) mfa)
  apply (Roll mfg) fa = Roll (map (\fg -> apply fg fa) mfg)

instance applicativeFree :: Functor m => Applicative (Free m) where
  -- pure :: forall a. a -> m a
  pure a = Pure a

instance bindFree :: Functor m => Bind (Free m) where
  -- bind :: forall a b. m a -> (a -> m b) -> m b
  bind (Pure a) k = k a
  bind (Roll mfa) k = Roll (map (\fa -> bind fa k) mfa)

instance monadFree :: Functor m => Monad (Free m)
