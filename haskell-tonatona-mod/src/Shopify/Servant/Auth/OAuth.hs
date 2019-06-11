{-# LANGUAGE TypeOperators #-}
module Shopify.Servant.Auth.OAuth where

import RIO

import Servant
import Shopify.Api.Admin.OAuth.Data.AccessToken (HasAccessToken, accessTokenL, AccessToken)
import Servant.Client.Core as Client (Request, AuthenticatedRequest, AuthClientData, mkAuthenticatedRequest, addHeader)

-- | Outbound Request

type instance AuthClientData (AuthProtect "shopify-access-token") = AccessToken

authenticateReq :: AccessToken -> Client.Request -> Client.Request
authenticateReq = Client.addHeader "X-Shopify-Access-Token" . Just

mkShopifyAuthenticateReq
  :: HasAccessToken env
  => RIO env (AuthenticatedRequest (AuthProtect "shopify-access-token"))
mkShopifyAuthenticateReq = do
  token <- view accessTokenL
  pure $ mkAuthenticatedRequest token authenticateReq

-- TODO Shopify Options, Resources
-- TODO ResponseJSON
-- newtype ResponseJSON a = ResponseJSON (Either ResponseSlackError a)
-- instance FromJSON a => FromJSON (ResponseJSON a) where
--     parseJSON = withObject "Response" $ \o -> do
--         ok <- o .: "ok"
--         ResponseJSON <$> if ok
--            then Right <$> parseJSON (Object o)
--            else Left . ResponseSlackError <$> o .: "error"
-- run
--   :: (MonadReader env m, HasManager env, MonadIO m)
--   => ClientM (ResponseJSON a)
--   -> m (Response a)
-- run clientAction = do
--   env <- ask
--   let baseUrl = BaseUrl Https "slack.com" 443 "/api"
--   unnestErrors <$> liftIO (runClientM clientAction $ mkClientEnv (getManager env) baseUrl)



