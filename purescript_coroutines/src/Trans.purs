module Free.Trans where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Functor (class Functor)
import Data.Exists (Exists, mkExists, runExists)

{- | Types -}
data FreeT f m a
  = FreeT
      (Unit -> -- one lazy step
         m -- any Monad interleaved with our Free Monad
           (FreeM f m a))
  | Bind (Exists (Bound f m a)) -- instead of stacking `bind` operators in nested closures each of which requires a separated stack allocation during evaluation, we pack them into data structures (which will be translated into POJOs in JS by purescript compiler) so later in the evaluation (`resume`, `runFreeT`) we can control the stack usage to be constant

type FreeM f m a=
  Either -- Free f a
    a -- Pure a
    (f (FreeT f m a)) -- Roll (f (Free f a))

-- `i` is the type of any intermediate result which will be hided by `Exists` (existential)
data Bound f m a i
  -- bind :: forall w i a. Monad w =>  w i -> (i -> w a) -> w a
  = Bound
      (Unit -> -- one lazy step
         FreeT f m i) -- w i = (FreeT f m) i
      (i -> FreeT f m a) -- (i -> w a) = (i -> (FreeT f m) a)

{- | Constructors -}

-- | private constructor for stack-safe `bind`
bound :: forall f m a i. (Unit -> FreeT f m i) -> (i -> FreeT f m a) -> FreeT f m a
bound i f = Bind $ mkExists $ Bound i f

-- | public constructor
freeT :: forall f m a. (Unit -> m (FreeM f m a)) -> FreeT f m a
freeT = FreeT

{- | Type Classes instances -}

instance functorFreeT :: (Functor f, Functor m) => Functor (FreeT f m) where
  map f (FreeT m) = FreeT $ const $ (bimap f (map (map f))) <$> m unit
  map f (Bind e) = flap(runExists) e \(Bound ui k) -> bound ui (map f <<< k)

instance applyFreeT :: (Functor f, Monad m) => Apply (FreeT f m) where
  apply = ap

instance applicativeFreeT :: (Functor f, Monad m) => Applicative (FreeT f m) where
  pure a = FreeT $ const $ pure $ Left a

instance bindFreeT :: (Functor f, Monad m) => Bind (FreeT f m) where
  bind (Bind e) kab = flap(runExists) e \(Bound ui kia) ->
    bound ui (\i -> bound (const $ kia i) kab)
  bind a kab = bound (const a) kab

instance monadFreeT :: (Functor f, Monad m) => Monad (FreeT f m)

instance monadTransFreeT :: (Functor f) => MonadTrans (FreeT f) where
  lift ma = FreeT \_ -> map Left ma

instance monadRecFreeT :: (Functor f, Monad m) => MonadRec (FreeT f m) where
  tailRecM f = go
    where
      go s =
        f s >>= case _ of
          Loop s1 -> go s1
          Done a -> pure a


{- | Operators -}

-- | unpack `FreeT`
resume :: forall f m a. Functor f => MonadRec m => FreeT f m a -> m (FreeM f m a)
      -- tailRecM :: forall m a b. MonadRec m => (a -> m (Step a b)) -> a -> m b
      -- a = (FreeT f m a), b = FreeM f m a
resume = tailRecM go
  where
    go :: FreeT f m a -> m (Step (FreeT f m a) (FreeM f m a))
    go (FreeT m) = map Done (m unit)
                              -- forall i. k :: i -> FreeT f m a
    go (Bind e) = flap(runExists) e \(Bound ufi kia) ->
      -- forall i.
      case (ufi unit) of
        FreeT umi ->
          let
            mi = umi unit
          in
           -- mi :: m (FreeM f m i)
                -- case :: FreeM f m i -> m (Step (FreeT f m a) (FreeM f m a))
            mi >>= case _ of
                                -- (k i) :: FreeT f m a
              Left i -> pure (Loop (kia i))
                 -- fc :: f (Free f m i)
                              -- (_ >>= k) :: FreeT f m i -> FreeT f m a
                          -- map (_ >>= k) :: f( FreeT f m i) -> f ( FreeT f m a)
              Right fi -> pure (Done (Right (map (_ >>= kia) fi)))
        Bind e1 -> flap(runExists) e1 \(Bound ufj kji) ->
          -- forall j.
          let
         -- fj :: FreeT f m j
            fj = ufj unit
          in
            pure (Loop (fj >>= kji >>= kia ))

runFreeT :: forall f m a. Functor f => MonadRec m => (f (FreeT f m a) -> m (FreeT f m a)) -> FreeT f m a -> m a
runFreeT interp = tailRecM (go <=< resume)
  where
    go :: Either a (f (FreeT f m a)) -> m (Step (FreeT f m a) a)
    go (Left a) = pure (Done a)
    go (Right fc) = Loop <$> interp fc

liftFreeT :: forall f m a. Functor f => Monad m => f a -> FreeT f m a
liftFreeT fa = FreeT $ const $ pure $ Right $ pure <$> fa
