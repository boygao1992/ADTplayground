module ContT where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Control.Monad.Trans.Class (class MonadTrans)

class Monad m <= MonadCont m where
  callCC :: forall a. ((forall b. a -> m b) -> m a) -> m a

-- | r = result
newtype ContT r m a = ContT ((a -> m r) -> m r)

derive instance newtypeContT :: Newtype (ContT r m a) _

runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
runContT (ContT c)= c

mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f (ContT c)= ContT $ f <<< c

cmapContT
  :: forall r m a b
  .  ((b -> m r) -> (a -> m r))
  -> ContT r m a
  -> ContT r m b
cmapContT f (ContT c) = ContT $ c <<< f

instance functorContT :: Functor m => Functor (ContT r m) where
  map :: forall a b. (a -> b) -> (ContT r m a) -> (ContT r m b)
                         -- c <<< (\k -> k <<< f) :: (b -> m r) -> m r
                         -- c :: (a -> m r) -> m r
                                -- \k -> k <<< f :: (b -> m r) -> (a -> m r)
                                      -- k <<< f :: a -> m r
                                 -- k :: b -> m r
                                            -- f :: a -> b
  map f (ContT c) = ContT $ c <<< (\k -> k <<< f)

instance applyContT :: Apply m => Apply (ContT r m) where
  apply :: forall a b. ContT r m (a -> b) -> ContT r m a -> ContT r m b
                                 -- \k -> f(\g -> v (k <<< g)) :: (b -> m r) -> m r
                                       -- f(\g -> v (k <<< g)) :: m r
            -- f :: ((a -> b) -> m r) -> m r
                                         -- \g -> v (k <<< g) :: (a -> b) -> m r
                                               -- v (k <<< g) :: m r
                      -- v :: (a -> m r) -> m r
                                                  -- k <<< g :: a -> m r
                                  -- k :: b -> m r
                                          -- g :: a -> b
  apply (ContT f) (ContT v) = ContT \k -> f(\g -> v (k <<< g))

instance applicativeContT :: Applicative m => Applicative (ContT r m) where
  pure :: forall a. a -> ContT r m a
                -- \k -> k x :: (a -> m r) -> m r
                      -- k x :: m r
                 -- k :: a -> m r
    -- x :: a
  pure x = ContT \k -> k x

instance bindContT :: Bind m => Bind (ContT r m) where
  bind :: forall a b. ContT r m a -> (a -> ContT r m b) -> ContT r m b
                        -- \l -> c (\x -> (unwrap <<< k $ x) l) :: (b -> m r) -> m r
                              -- c (\x -> (unwrap <<< k $ x) l) :: m r
           -- c :: (a -> m r) -> m r
                                 -- \x -> (unwrap <<< k $ x) l :: a -> m r
                                       -- (unwrap <<< k $ x) l :: m r
                         -- l :: b -> m r
                                        -- unwrap <<< k $ x :: (b -> m r) -> m r
                                  -- x :: a
                                        -- unwrap <<< k :: a -> ((b -> m r) -> m r)
              -- k :: (a -> ContT r m b)
  bind (ContT c) k = ContT \l -> c (\x -> (unwrap <<< k $ x) l)

instance monadContT :: Monad m => Monad (ContT r m)

instance monadContContT :: Monad m => MonadCont (ContT r m) where
  callCC :: forall a. ((forall b. a -> ContT r m b) -> ContT r m a) -> ContT r m a
                -- \k -> unwrap (f (\x -> ContT (const $ k x))) $ k :: (a -> m r) -> m r
                      -- unwrap (f (\x -> ContT (const $ k x))) $ k :: m r
                 -- k :: a -> m r
                      -- unwrap (f (\x -> ContT (const $ k x))) :: (a -> m r) -> m r
                              -- f (\x -> ContT (const $ k x)) :: ContT r m a
      -- f :: forall a. (forall b. a -> ContT r m b) -> ContT r m a
                                 -- \x -> ContT (const $ k x) :: forall b. a -> ContT r m b
                                       -- ContT (const $ k x) :: forall b. ContT r m b
                                              -- const $ k x :: forall b. (b -> m r) -> m r
                                                      -- k a :: m r
                                  -- x :: a
                 -- k :: a -> m r
  callCC f = ContT \k -> unwrap (f (\x -> ContT (const $ k x))) $ k

instance monadTransContT :: MonadTrans (ContT r) where
  lift m = ContT (_ =<< m)
