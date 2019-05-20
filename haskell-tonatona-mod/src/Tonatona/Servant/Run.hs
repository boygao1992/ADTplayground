{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Tonatona.Servant.Run where

import RIO

import Network.HTTP.Types.Header (hLocation)
import Servant (Application, Handler, HasServer, ServantErr, ServerT, errHeaders, err302, hoistServer, serve, throwError)

import Tonatona.Servant.Resources (HasServantResources, servantResourcesL, _loggerMiddleware, _runApplication)

runServantServer
  :: forall (api :: *) env.
  ( HasServer api '[]
  , HasServantResources env
  )
  => ServerT api (RIO env)
  -> RIO env ()
runServantServer servantServer = do
  loggerMiddleware <- view (servantResourcesL._loggerMiddleware)
  runApplication <- view (servantResourcesL._runApplication)
  initApp <- runServant servantServer
  let app = loggerMiddleware initApp
  liftIO $ runApplication app

  where
    runServant :: ServerT api (RIO env) -> RIO env Application
    runServant server = do
      env <- ask
      pure
        $ serve (Proxy @api)
        $ hoistServer (Proxy @api) (transformation env) server

    transformation :: forall a. env -> RIO env a -> Servant.Handler a
    transformation env action = do
      let ioAction = Right <$> runRIO env action
#if MIN_VERSION_servant(0, 16, 0)
      eitherRes <- liftIO $ ioAction `catch` \(e :: ServerError) -> pure $ Left e
#else
      eitherRes <- liftIO $ ioAction `catch` \(e :: ServantErr) -> pure $ Left e
#endif
      case eitherRes of
        Right res -> pure res
        Left servantErr -> throwError servantErr

redirect :: ByteString -> RIO env a
redirect redirectLocation =
  throwM $
    err302
      { errHeaders = [(hLocation, redirectLocation)]
      }
