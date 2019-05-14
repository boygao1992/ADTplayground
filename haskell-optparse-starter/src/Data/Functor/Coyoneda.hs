module Data.Functor.Coyoneda where

import Import

data Coyoneda f a where
  Coyoneda :: (i -> a) -> f i -> Coyoneda f a

liftCoyoneda :: f a -> Coyoneda f a
liftCoyoneda = Coyoneda id
{-# INLINE liftCoyoneda #-}

lowerCoyoneda :: Functor f => Coyoneda f a -> f a
lowerCoyoneda (Coyoneda ia fi) = fmap ia fi
{-# INLINE lowerCoyoneda #-}

hoistCoyoneda :: (forall i. f i -> g i) -> (Coyoneda f a -> Coyoneda g a)
hoistCoyoneda nat (Coyoneda ia fi) = Coyoneda ia (nat fi)
{-# INLINE hoistCoyoneda #-}

instance Functor (Coyoneda f) where
  fmap ab (Coyoneda ia fi) = Coyoneda (ab . ia) fi
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Coyoneda f) where
  pure :: a -> Coyoneda f a
  pure = liftCoyoneda . pure
  {-# INLINE pure #-}

  (<*>) :: Coyoneda f (a -> b) -> Coyoneda f a -> Coyoneda f b
  fab <*> fa = liftCoyoneda $ lowerCoyoneda fab <*> lowerCoyoneda fa
  {-# INLINE (<*>) #-}

  (*>) :: Coyoneda f a -> Coyoneda f b -> Coyoneda f b
  Coyoneda _ fi *> Coyoneda jb fj = Coyoneda jb (fi *> fj)
  {-# INLINE (*>) #-}

  (<*) :: Coyoneda f a -> Coyoneda f b -> Coyoneda f a
  Coyoneda ia fi <* Coyoneda _ fj = Coyoneda ia (fi <* fj)
  {-# INLINE (<*) #-}

instance Monad m => Monad (Coyoneda m) where
  (>>=) :: Coyoneda m a -> (a -> Coyoneda m b) -> Coyoneda m b
  Coyoneda ia mi >>= k = liftCoyoneda $ mi >>= (lowerCoyoneda . k . ia)

