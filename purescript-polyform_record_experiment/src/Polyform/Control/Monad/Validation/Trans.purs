module Control.Monad.Validation.Trans where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)
import Control.Plus (class Plus)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))

{-

data V r o      | type V r o = Monoid r =>
  = Invalid r   |   { writer :: r
  | Valid r o   |   , maybe :: Maybe o
                |   }

newtype V r o = V (Tuple (Maybe o) r)

newtype ValidatedT r m o = ValidatedT (MaybeT (WriterT r m) o)
-------------------------------------------------------------------------------
newtype ValidatedT r m o = ValidatedT (WriterT r m (Maybe o))
-------------------------------------------------------------------------------
newtype ValidatedT r m o = ValidatedT (m (Tuple (Maybe o) r))

newtype ValidationT i r m o = ValidationT (MaybeT (WriterT r (ReaderT i m)) o)
-------------------------------------------------------------------------------
newtype ValidationT i r m o = ValidationT (ValidatedT r (ReaderT i m) o)
-------------------------------------------------------------------------------
newtype ValidationT i r m o = ValidationT (ReaderT i (m (Tuple (Maybe o) r)))
-------------------------------------------------------------------------------
newtype ValidationT i r m o = ValidationT (i -> (m (Tuple (Maybe o) r)))
-------------------------------------------------------------------------------
newtype ValidationT i r m o = ValidationT (i -> ValidatedT r m o))
-------------------------------------------------------------------------------
newtype ValidationT i r m o = ValidationT (Star (ValidatedT r m) i o)

-}

-- | ValidatedT

newtype ValidatedT r m o = ValidatedT (MaybeT (WriterT r m) o)
derive instance newtypeValidatedT :: Newtype (ValidatedT r m o) _
derive newtype instance functorValidatedT
  :: Functor m => Functor (ValidatedT r m)

runValidatedT
  :: forall r m o. ValidatedT r m o -> m (Tuple (Maybe o) r)
runValidatedT = runWriterT <<< runMaybeT <<< unwrap

instance applyValidatedT
  :: (Semigroup r, Monad m) => Apply (ValidatedT r m)
  where
    apply vmwm1 vmwm2 = ValidatedT <<< MaybeT <<< WriterT $ do
      t1 <- runValidatedT vmwm1
      t2 <- runValidatedT vmwm2
-- NOTE instance Semigroup a => Apply (Tuple a)
--      instance Apply Maybe
--      DRY, but too many layers of indirection, almost not human-readable and less efficient
--    pure <<< swap $ (<*>) <$> (swap t1) <*> (swap t2)
      pure $ go t1 t2

      where
        go (Tuple (Just f) r1) (Tuple (Just x) r2)  = Tuple (Just $ f x) (r1 <> r2)
        go (Tuple _ r1) (Tuple _ r2) = Tuple Nothing (r1 <> r2)

derive newtype instance applicativeV
  :: (Monoid r, Monad m) => Applicative (ValidatedT r m)

-- NOTE not law-abiding, (<*>) /= ap
-- [Data.Validation](http://hackage.haskell.org/package/validation-1/docs/Data-Validation.html)
-- A consequence of this is that Validation has no Bind or Monad instance. This is because such an instance would violate the law that a Monad's ap must equal the Applicative's <*>
-- instance bindValidatedT
--   :: (Semigroup r, Monad m) => Bind (ValidatedT r m)
--   where
--     bind vmwm k = ValidatedT <<< MaybeT <<< WriterT $
--       vmwm # runValidatedT >>> runMaybeT >>> runWriterT >>= case _ of
--         Tuple Nothing r1 -> pure (Tuple Nothing r1)
--         Tuple (Just x) r1 ->
--           k x # runValidatedT >>> runMaybeT >>> runWriterT >>= case _ of
--             Tuple Nothing r2 -> pure (Tuple Nothing (r1 <> r2))
--             Tuple (Just y) r2 -> pure (Tuple (Just y) (r1 <> r2))
-- instance monadValidatedT
--   :: (Monoid r, Monad m) => Monad (ValidatedT r m)

instance altValidatedT
  :: (Semigroup r, Monad m) => Alt (ValidatedT r m)
  where
    alt vmwm1 vmwm2 = ValidatedT <<< MaybeT <<< WriterT $ do
      t1 <- runValidatedT vmwm1
      t2 <- runValidatedT vmwm2
      pure $ go t1 t2
      where
        go (Tuple (Just x) r1) (Tuple _ r2) = Tuple (Just x) (r1 <> r2)
        go (Tuple _ r1) (Tuple (Just y) r2) = Tuple (Just y) (r1 <> r2)
        go (Tuple Nothing r1) (Tuple Nothing r2) = Tuple Nothing (r1 <> r2)

derive newtype instance plusV :: (Monoid r, Monad m) => Plus (ValidatedT r m)

-- | ValidationT

newtype ValidationT i r m o = ValidationT (ValidatedT r (ReaderT i m) o)
derive instance newtypeValidationT :: Newtype (ValidationT i r m o) _
derive newtype instance functorValidationT
  :: Functor m => Functor (ValidationT i r m)

runValidationT
  :: forall i r m o. ValidationT i r m o -> (i -> m (Tuple (Maybe o) r))
runValidationT = runReaderT <<< runValidatedT <<< unwrap

derive newtype instance applyValidationT
  :: (Semigroup r, Monad m) => Apply (ValidationT i r m)

derive newtype instance applicativeValidationT
  :: (Monoid r, Monad m) => Applicative (ValidationT i r m)

-- NOTE not law-abiding, (<*>) /= ap
-- derive newtype instance bindValidationT
--   :: (Monoid r, Monad m) => Bind (ValidationT i r m)
-- derive newtype instance monadValidationT
--   :: (Monoid r, Monad m) => Monad (ValidationT i r m)

derive newtype instance altValidationT
  :: (Semigroup r, Monad m) => Alt (ValidationT i r m)

derive newtype instance plusValidationT
  :: (Monoid r, Monad m) => Plus (ValidationT i r m)
