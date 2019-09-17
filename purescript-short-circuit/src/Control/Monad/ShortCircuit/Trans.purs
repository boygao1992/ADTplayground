module Control.Monad.ShortCircuit.Trans where

import Prelude

import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Except.Trans (class MonadError, class MonadThrow, ExceptT, runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.Plus (empty)
import Data.Either (Either(..))
import Effect.Class (class MonadEffect)

newtype ShortCircuitT o m a = ShortCircuitT (ExceptT o m a)
derive instance functorShortCircuitT :: Functor m => Functor (ShortCircuitT o m)
derive newtype instance applyShortCircuitT :: Monad m => Apply (ShortCircuitT o m)
derive newtype instance bindShortCircuitT :: Monad m => Bind (ShortCircuitT o m)
derive newtype instance applicativeShortCircuitT :: Monad m => Applicative (ShortCircuitT o m)
derive newtype instance monadShortCircuitT :: Monad m => Monad (ShortCircuitT o m)
derive newtype instance monadRecShortCircuitT :: MonadRec m => MonadRec (ShortCircuitT o m)
derive newtype instance monadTransShortCircuitT :: MonadTrans (ShortCircuitT o)

derive newtype instance monadEffectShortCircuitT :: MonadEffect m => MonadEffect (ShortCircuitT e m)

derive newtype instance monadContShortCircuitT :: MonadCont m => MonadCont (ShortCircuitT e m)

derive newtype instance monadThrowShortCircuitT :: Monad m => MonadThrow e (ShortCircuitT e m)

derive newtype instance monadErrorShortCircuitT :: Monad m => MonadError e (ShortCircuitT e m)

derive newtype instance monadAskShortCircuitT :: MonadAsk r m => MonadAsk r (ShortCircuitT e m)

derive newtype instance monadReaderShortCircuitT :: MonadReader r m => MonadReader r (ShortCircuitT e m)

derive newtype instance monadStateShortCircuitT :: MonadState s m => MonadState s (ShortCircuitT e m)

derive newtype instance monadTellShortCircuitT :: MonadTell w m => MonadTell w (ShortCircuitT e m)

derive newtype instance monadWriterShortCircuitT :: MonadWriter w m => MonadWriter w (ShortCircuitT e m)

runShortCircuitT :: forall o m a. Functor m => ShortCircuitT o m a -> m (Either a o)
runShortCircuitT (ShortCircuitT e) = flipEither <$> runExceptT e
  where
    flipEither :: Either o a -> Either a o
    flipEither = case _ of
      Left o -> Right o
      Right a -> Left a

class MonadBreak m where
  break :: forall a. m a

instance monadBreakMaybeT :: (Monad m) => MonadBreak (MaybeT m) where
  break = empty
instance monadBreakShortCircuitT :: (Monoid o, Monad m) => MonadBreak (ShortCircuitT o m) where
  break = ShortCircuitT $ empty

class MonadReturn o m where
  return :: forall a. o -> m a

instance monadReturnShortCircuitT :: Monad m => MonadReturn o (ShortCircuitT o m) where
  return = ShortCircuitT <<< throwError

