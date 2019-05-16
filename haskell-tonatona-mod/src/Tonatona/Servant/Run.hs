{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Tonatona.Servant.Run where

import RIO

import Network.HTTP.Types.Header (hLocation)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant (Application, Handler, HasServer, ServantErr, ServerT, errHeaders, err302, hoistServer, serve, throwError)

import Tonatona.Servant.Options (HasServantOptions, servantOptionsL, _port)
import Tonatona.Logger.Options (HasLoggerOptions, LoggerOptions(..), loggerOptionsL, defaultVerbosity)

run
  :: forall (api :: *) env.
  ( HasServer api '[]
  , HasServantOptions env
  , HasLoggerOptions env
  )
  => ServerT api (RIO env)
  -> RIO env ()
run servantServer = do
  env <- ask
  port <- view (servantOptionsL . _port)
  loggingMiddleware <- reqLogMiddleware
  let
    app = loggingMiddleware $ runServant env
    settings = Warp.setPort port $ Warp.defaultSettings
  liftIO $ Warp.runSettings settings app

  where
    runServant :: env -> Application
    runServant env =
      serve (Proxy @api) $ hoistServer (Proxy @api) transformation servantServer
      where
        transformation :: forall a. RIO env a -> Servant.Handler a
        transformation action = do
          let ioAction = Right <$> runRIO env action
#if MIN_VERSION_servant(0, 16, 0)
          eitherRes <- liftIO $ ioAction `catch` \(e :: ServerError) -> pure $ Left e
#else
          eitherRes <- liftIO $ ioAction `catch` \(e :: ServantErr) -> pure $ Left e
#endif
          case eitherRes of
            Right res -> pure res
            Left servantErr -> throwError servantErr

    reqLogMiddleware :: RIO env Middleware
    reqLogMiddleware = do
      LoggerOptions { mode, verbose } <- view loggerOptionsL
      pure $ if defaultVerbosity mode verbose
        then logStdoutDev
        else logStdout

redirect :: ByteString -> RIO env a
redirect redirectLocation =
  throwM $
    err302
      { errHeaders = [(hLocation, redirectLocation)]
      }
