module Shopify.Api.Admin.OAuth.Data.Req.OAuthAuthorize where

import RIO

import Shopify.Api.Admin.Data.Scopes (Scopes)
import Shopify.Api.Admin.OAuth.Data.AccessMode (AccessMode)

data Req = Req
  { _api_key :: Maybe Text
  , _scopes :: Maybe Scopes
  , _redirect_url :: Maybe Text
  , _nonce :: Maybe Text
  , _access_modes :: [AccessMode]
  } deriving (Eq, Show)

emptyReq :: Req
emptyReq = Req Nothing Nothing Nothing Nothing []
