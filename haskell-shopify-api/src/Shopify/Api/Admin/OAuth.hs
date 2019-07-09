{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Admin.OAuth where

import RIO
import Servant
import Shopify.Servant.Client.Util (getBaseClients)

import Shopify.Api.Admin.Data.Scopes (Scopes)
import Shopify.Api.Admin.OAuth.Data.AccessMode (AccessMode)
import Shopify.Api.Admin.OAuth.Data.Req.OAuthAuthorize as OAuthAuthorize (Req(..))
import Shopify.Api.Admin.OAuth.Data.Req.OAuthAccessToken as OAuthAccessToken (Req(..), Res(..))

import Shopify.TestApp.Types (BaseHttpClientResources)

-- | Redirect Url

-- https://{shop}.myshopify.com/admin/oauth/authorize?client_id={api_key}&scope={scopes}&redirect_uri={redirect_uri}&state={nonce}&grant_options[]={access_mode}
type OAuthAuthorize
  = "admin" :> "oauth" :> "authorize"
  :> QueryParam "client_id" Text
  :> QueryParam "scope" Scopes
  :> QueryParam "redirect_uri" Text -- TODO redirect_url = base_url + install_url
  :> QueryParam "state" Text
  :> QueryParams "grant_options" AccessMode
  :> Post '[JSON] NoContent

oAuthAuthorizeUrl :: Text -> OAuthAuthorize.Req -> Text
oAuthAuthorizeUrl
  baseUrl
  (OAuthAuthorize.Req api_key scopes redirect_url nonce access_modes)
  = baseUrl <> _oAuthAuthorizeUrl api_key scopes redirect_url nonce access_modes
  where
    _oAuthAuthorizeUrl
      :: Maybe Text -> Maybe Scopes -> Maybe Text -> Maybe Text -> [AccessMode] -> Text
    _oAuthAuthorizeUrl = allLinks' toUrlPiece (Proxy @OAuthAuthorize)

-- | Get Access Token Request

-- oAuthAccessTokenBasePath :: String
-- oAuthAccessTokenBasePath = "admin/oauth/access_token"

type OAuthAccessToken
  = "admin" :> "oauth" :> "access_token"
  :> ReqBody '[JSON] OAuthAccessToken.Req
  :> Post '[JSON] OAuthAccessToken.Res

getAccessToken :: OAuthAccessToken.Req -> RIO BaseHttpClientResources Res
getAccessToken = getBaseClients (Proxy @OAuthAccessToken)
