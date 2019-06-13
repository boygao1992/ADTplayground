module Shopify.Servant.Client.Auth.OAuth where

import Servant
import Servant.Client.Core as Client (Request, AuthenticatedRequest, AuthClientData, mkAuthenticatedRequest, addHeader)
import Shopify.Api.Admin.OAuth.Data.AccessToken (AccessToken)

-- | Outbound Request

type instance AuthClientData (AuthProtect "shopify-access-token") = AccessToken

authenticateReq :: AccessToken -> Client.Request -> Client.Request
authenticateReq = Client.addHeader "X-Shopify-Access-Token"

mkShopifyAuthenticateReq
  :: AccessToken -> AuthenticatedRequest (AuthProtect "shopify-access-token")
mkShopifyAuthenticateReq token = mkAuthenticatedRequest token authenticateReq

