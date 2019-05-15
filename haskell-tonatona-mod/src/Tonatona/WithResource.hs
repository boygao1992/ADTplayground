module Tonatona.WithResource where

import RIO

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Cont (ContT(..))

class With options resource where
  withResource :: MonadUnliftIO m => WithResource options m resource

newtype WithResource options m resource
  = WithResource (ReaderT options (ContT () m) resource)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

hoistWithResource :: (options -> ((resource -> m ()) -> m ())) -> WithResource options m resource
hoistWithResource action = WithResource $ ReaderT $ ContT <$> action

-- data AppOptions = AppOptions
--   { appLoggerOptions :: LoggerOptions
--   , appBeamPostgresqlOptions :: BeamPostgresqlOptions
--   }
-- instance HasLoggerOptions AppOptions where
--   loggerOptions = lens appLoggerOptions (\x y -> x { appLoggerOptions = y })
-- instance HasBeamPostgresqlOptions AppOptions where
--   beamPostgresqlOptions = lens appBeamPostgresqlOptions (\x y -> x { appBeamPostgresqlOptions = y })

-- data Config = Config
--   { logFunc :: LogFunc
--   , beamPostgresqlConnection :: Connection
--   }

-- instance With AppOptions Config where
--   withResource = Config <$> withResource <*> withResource
