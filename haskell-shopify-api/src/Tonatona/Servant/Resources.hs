module Tonatona.Servant.Resources where

import RIO

import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Gzip (gzip, def, gzipFiles, GzipFiles(..))
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Servant (Application)

import Tonatona.Logger.Options (HasLoggerOptions, loggerOptionsL, _mode, _verbose, defaultVerbosity)
import Tonatona.Servant.Options (HasServantOptions, Protocol(..), servantOptionsL, _host, _port, _logging, _gzip, _protocol, _tlsCertFilePath, _tlsCertKeyFilePath, _tlsCertChainFilePaths)
import Tonatona.WithResource (With, withResource, hoistWithResource)

emptyMiddleware :: Middleware -- NOTE = Wai.Application -> Wai.Application
emptyMiddleware = id

class HasServantResources resources where
  servantResourcesL :: Lens' resources ServantResources

-- Resource Initialization
data ServantResources = ServantResources
  { servantLoggerMiddleware :: !ServantRequestLoggerMiddleware
  , servantRunApplication :: !ServantRunApplication
  , servantGzipMiddleware :: !ServantGzipMiddleware
  }
instance ( HasLoggerOptions options
         , HasServantOptions options
         ) => With options ServantResources where
  withResource = ServantResources <$> withResource <*> withResource <*> withResource
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

newtype ServantGzipMiddleware
  = ServantGzipMiddleware { unGzipMiddleware :: Middleware }
_gzipMiddleware :: Lens' ServantResources Middleware
_gzipMiddleware = lens (unGzipMiddleware . servantGzipMiddleware) \x y -> x { servantGzipMiddleware = ServantGzipMiddleware y }
instance HasServantOptions options => With options ServantGzipMiddleware where
  withResource = hoistWithResource \option cont -> do
    let compressing = option^.servantOptionsL._gzip
    cont . ServantGzipMiddleware
      $ if
        | compressing -> gzip (def { gzipFiles = GzipCompress })
        | otherwise -> emptyMiddleware

newtype ServantRunApplication = ServantRunApplication
  { unRunApplication :: Application -> IO () }
_runApplication :: Lens' ServantResources (Application -> IO ())
_runApplication = lens (unRunApplication . servantRunApplication) \x y -> x { servantRunApplication = ServantRunApplication y }
instance ( HasServantOptions options
         ) => With options (ServantRunApplication) where
  withResource = hoistWithResource \options cont -> do
    let protocol = options^.servantOptionsL._protocol
        host = options^.servantOptionsL._host
        port = options^.servantOptionsL._port
        settings =
          Warp.setPort port
          . Warp.setHost host
          $ Warp.defaultSettings
    cont . ServantRunApplication
      $ if protocol == Https
        then
          let
            cert = options^.servantOptionsL._tlsCertFilePath
            chains = options^.servantOptionsL._tlsCertChainFilePaths
            key = options^.servantOptionsL._tlsCertKeyFilePath
            tlsSettings = WarpTLS.tlsSettingsChain cert chains key
          in
            WarpTLS.runTLS tlsSettings settings
        else Warp.runSettings settings
