module Control.Monad.Validation.Trans where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Polyform.Validation (Validation(..))


{-

data V r o      | type V r o = Monoid r =>
  = Invalid r   |   { writer :: r
  | Valid r o   |   , maybe :: Maybe o
                |   }

newtype V r o = V (Tuple (Maybe o) r)

newtype ValidationResultT r m o = ValidationResultT (MaybeT (WriterT r m) o)
-------------------------------------------------------------------------------
newtype ValidationResultT r m o = ValidationResultT (WriterT r m (Maybe o))
-------------------------------------------------------------------------------
newtype ValidationResultT r m o = ValidationResultT (m (Tuple (Maybe o) r))

newtype ValidationT i r m o = ValidationT (MaybeT (WriterT r (ReaderT i m)) o)
-------------------------------------------------------------------------------
newtype ValidationT i r m o = ValidationT (ValidationResultT r (ReaderT i m) o)
-------------------------------------------------------------------------------
newtype ValidationT i r m o = ValidationT (ReaderT i (m (Tuple (Maybe o) r)))
-------------------------------------------------------------------------------
newtype ValidationT i r m o = ValidationT (i -> (m (Tuple (Maybe o) r)))

-}

newtype ValidationResultT r m o = ValidationResultT (MaybeT (WriterT r m) o)
derive instance newtypeValidationResultT :: Newtype (ValidationResultT r m o) _
derive newtype instance functorValidationResultT
  :: Functor m => Functor (ValidationResultT r m)

runValidationResultT
  :: forall r m o. ValidationResultT r m o -> MaybeT (WriterT r m) o
runValidationResultT (ValidationResultT v) = v

instance applyValidationResultT
  :: (Semigroup r, Monad m) => Apply (ValidationResultT r m)
  where
    apply vmwm1 vmwm2 = ValidationResultT <<< MaybeT <<< WriterT $ do
      m1 <- runWriterT <<< runMaybeT <<< runValidationResultT $ vmwm1
      m2 <- runWriterT <<< runMaybeT <<< runValidationResultT $ vmwm2
      pure $ go m1 m2

      where
        go (Tuple (Just f) r1) (Tuple (Just x) r2)  = Tuple (Just $ f x) (r1 <> r2)
        go (Tuple _ r1) (Tuple _ r2) = Tuple Nothing (r1 <> r2)

derive newtype instance applicativeV
  :: (Monoid r, Monad m) => Applicative (ValidationResultT r m)

-- instance bindValidationResultT :: Bind
