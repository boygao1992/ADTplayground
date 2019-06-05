{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Tonatona.Servant.Run where

import RIO

import Network.HTTP.Types.Header (hLocation)
import Servant (Application, Handler, HasServer, Header, Headers, JSON, PostCreated, NoContent(..), ServantErr, ServerT, ToHttpApiData, Verb, addHeader, errHeaders, err302, hoistServer, serve, throwError)

import Tonatona.Servant.Resources (HasServantResources, servantResourcesL, _loggerMiddleware, _runApplication, _gzipMiddleware)

runServantServer
  :: forall (api :: *) env.
  ( HasServer api '[]
  , HasServantResources env
  )
  => ServerT api (RIO env)
  -> RIO env ()
runServantServer servantServer = do
  loggerMiddleware <- view (servantResourcesL._loggerMiddleware)
  gzipMiddleware <- view (servantResourcesL._gzipMiddleware)
  runApplication <- view (servantResourcesL._runApplication)
  initApp <- runServant servantServer
  let
    app
      = gzipMiddleware
      $ loggerMiddleware
      $ initApp
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

-- NOTE not recommended
redirect' :: ByteString -> RIO env a
redirect' redirectLocation =
  throwM $
    err302
      { errHeaders = [(hLocation, redirectLocation)]
      }

type PostCreatedRedirect url -- 201
  = PostCreated '[JSON] (Headers '[Header "Location" url] NoContent)
type PermanentRedirect verb url
  = Verb verb 301 '[JSON] (Headers '[Header "Location" url] NoContent)
type TemporaryRedirect verb url
  = Verb verb 302 '[JSON] (Headers '[Header "Location" url] NoContent)
redirect
  :: ToHttpApiData url
  => url -- ^ what to put in the 'Location' header
  -> RIO env (Headers '[Header "Location" url] NoContent)
redirect url = return (addHeader url NoContent)

