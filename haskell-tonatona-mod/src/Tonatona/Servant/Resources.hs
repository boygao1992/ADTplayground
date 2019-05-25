module Tonatona.Servant.Resources where

import RIO

import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant (Application)

import Tonatona.Logger.Options (HasLoggerOptions, loggerOptionsL, _mode, _verbose, defaultVerbosity)
import Tonatona.Servant.Options (HasServantOptions, servantOptionsL, _host, _port, _logging)
import Tonatona.WithResource (With, withResource, hoistWithResource)

emptyMiddleware :: Middleware -- NOTE = Wai.Application -> Wai.Application
emptyMiddleware = id

class HasServantResources resources where
  servantResourcesL :: Lens' resources ServantResources

-- Resource Initialization
data ServantResources = ServantResources
  { servantLoggerMiddleware :: ServantRequestLoggerMiddleware
  , servantRunApplication :: ServantRunApplication
  }
instance ( HasLoggerOptions options
         , HasServantOptions options
         ) => With options ServantResources where
  withResource = ServantResources <$> withResource <*> withResource
instance HasServantResources ServantResources where
  servantResourcesL = id

newtype ServantRequestLoggerMiddleware
  = ServantRequestLoggerMiddleware { unLoggerMiddleware :: Middleware }
_loggerMiddleware :: Lens' ServantResources Middleware
_loggerMiddleware = lens (unLoggerMiddleware . servantLoggerMiddleware) \x y -> x { servantLoggerMiddleware = ServantRequestLoggerMiddleware y }
instance ( HasLoggerOptions options
         , HasServantOptions options
         ) => With options ServantRequestLoggerMiddleware where
  withResource = hoistWithResource \options cont -> do
    let mode = options^.loggerOptionsL._mode
        verbose = options^.loggerOptionsL._verbose
        logging = options^.servantOptionsL._logging
    cont . ServantRequestLoggerMiddleware
      $ if
        | logging -> if
          | defaultVerbosity mode verbose -> logStdoutDev
          | otherwise -> logStdout
        | otherwise -> emptyMiddleware

newtype ServantRunApplication = ServantRunApplication
  { unRunApplication :: Application -> IO () }
_runApplication :: Lens' ServantResources (Application -> IO ())
_runApplication = lens (unRunApplication . servantRunApplication) \x y -> x { servantRunApplication = ServantRunApplication y }
instance (HasServantOptions options
         ) => With options (ServantRunApplication) where
  withResource = hoistWithResource \options cont -> do
    let host = options^.servantOptionsL._host
        port = options^.servantOptionsL._port
        settings =
          Warp.setPort port
          . Warp.setHost host
          $ Warp.defaultSettings
    cont . ServantRunApplication
      $ Warp.runSettings settings
