{-# LANGUAGE AllowAmbiguousTypes #-}
module Shopify.Servant.Client.Util where

import RIO
import RIO.Text as Text
import Servant
import Servant.Client
import Tonatona.Servant.Run (getClients)
import Tonatona.Shopify.Options (HasShopifyOptions, shopifyOptionsL, _apiVersion)
import Tonatona.Shopify.Resources (HasShopifyResources, shopifyResourcesL, _shopifyHttpClientManager)

getRIOClients
  :: forall api env
  . ( HasClient ClientM api
    , HasShopifyResources env
    , HasShopifyOptions env
    , HasLogFunc env
    )
  => Proxy api
  -> Client (RIO env) api
getRIOClients hoistClientApi
  = hoistClient hoistClientApi
      transformation
      (client hoistClientApi)
  where
    transformation :: ClientM a -> RIO env a
    transformation cm = do
      apiVersion <- view (shopifyOptionsL._apiVersion)
      manager <- view (shopifyResourcesL._shopifyHttpClientManager)
      let
        baseUrl
        -- TODO data ShopifyHost = { shopifyHostname :: Text, shopifyApiRootPath :: Text }
        -- HasShopifyHost env where shopifyHostL :: Lens' env ShopifyHost
          = BaseUrl Https ("shopname" <> ".myshopify.com") 443 (Text.unpack $ "/admin/api/" <> apiVersion <> "/")
        clientEnv
          = mkClientEnv manager baseUrl
      e <- liftIO $ runClientM cm clientEnv
      case e of
        Left servantErr -> do
          logError $ displayShow servantErr
          throwM err500
        Right a -> pure a
