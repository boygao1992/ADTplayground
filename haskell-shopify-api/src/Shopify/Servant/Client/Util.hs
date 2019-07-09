{-# LANGUAGE TypeApplications #-}
module Shopify.Servant.Client.Util
  ( getBaseClients
  , getApiClients
  , runBaseHttpClient
  , runApiHttpClient
  , module Export
  ) where
import Shopify.Servant.Client.Auth.OAuth as Export (mkShopifyAuthenticateReq)

import RIO
import Servant.Client (HasClient, ClientM, Client)
import Servant.Client.Core (BaseUrl(..), Scheme(..))

import qualified Tonatona.Servant.Run as Servant (getClients)
import Tonatona.Servant.Run (mkClientEnv)
import Tonatona.Shopify.Options (HasShopifyOptions, shopifyOptionsL, _apiVersion)
import Tonatona.Shopify.Resources (shopifyResourcesL, _manager)

import Shopify.Api.Admin.OAuth.Data.AccessToken (AccessToken)
import Shopify.TestApp.Types (BaseHttpClientResources(..), ApiHttpClientResources(..), Resources)

getBaseClients
  :: forall api
  . HasClient ClientM api
  => Proxy api
  -> Client (RIO BaseHttpClientResources) api
getBaseClients = Servant.getClients @api @BaseHttpClientResources

getApiClients
  :: forall api
  . HasClient ClientM api
  => Proxy api
  -> Client (RIO ApiHttpClientResources) api
getApiClients = Servant.getClients @api @ApiHttpClientResources

mkRootBaseUrl :: String -> BaseUrl
mkRootBaseUrl hostname = BaseUrl Https hostname 443 ""

runBaseHttpClient
  :: String
  -> RIO BaseHttpClientResources a
  -> RIO Resources a
runBaseHttpClient hostname clientReq = do
  resources <- ask
  manager <- view (shopifyResourcesL._manager)
  let clientEnv = mkClientEnv manager (mkRootBaseUrl hostname)
  runRIO (BaseHttpClientResources resources clientEnv) clientReq

mkApiBaseUrl :: HasShopifyOptions env => String -> RIO env BaseUrl
mkApiBaseUrl hostname = do
  apiVersion <- view (shopifyOptionsL._apiVersion)
  pure $ BaseUrl Https hostname 443 ("admin/api/" <> apiVersion)

runApiHttpClient
  :: String
  -> AccessToken
  -> RIO ApiHttpClientResources a
  -> RIO Resources a
runApiHttpClient hostname token clientReq = do
  resources <- ask
  manager <- view (shopifyResourcesL._manager)
  baseUrl <- mkApiBaseUrl hostname
  let clientEnv = mkClientEnv manager baseUrl
  runRIO (ApiHttpClientResources resources clientEnv token) clientReq

