module Tonatona.WithResource where

import RIO

import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.Cont (ContT(..), runContT)

class With options resource where
  withResource :: MonadUnliftIO m => WithResource options m resource

newtype WithResource options m resource
  = WithResource (ReaderT options (ContT () m) resource)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

hoistWithResource :: (options -> ((resource -> m ()) -> m ())) -> WithResource options m resource
hoistWithResource action = WithResource $ ReaderT $ ContT <$> action

runWithResource ::
  ( With options resource
  , MonadUnliftIO m
  )
  => options
  -> (resource -> m ())
  -> m ()
runWithResource = _runWithResource withResource
  where
    _runWithResource :: WithResource options m resource -> options -> (resource -> m ()) -> m ()
    _runWithResource (WithResource w) opts action = runContT (runReaderT w opts) action

