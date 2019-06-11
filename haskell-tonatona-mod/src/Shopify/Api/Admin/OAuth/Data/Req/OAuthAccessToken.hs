{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Admin.OAuth.Data.Req.OAuthAccessToken where

import RIO
import Data.Aeson.TH

import Shopify.Api.Customer.Data.CustomerId (CustomerId)
import Shopify.Api.Admin.Data.Scopes (Scopes)
import Shopify.Api.Admin.OAuth.Data.AccessToken (AccessToken)

data Req = Req
  { _client_id :: !Text
  , _client_secret :: !Text
  , _code :: !Text
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Req)

data AssociatedUser = AssociatedUser
  { _id :: !(Maybe CustomerId)
  , _first_name :: !(Maybe Text)
  , _last_name :: !(Maybe Text)
  , _email :: !(Maybe Text)
  , _email_verified :: !(Maybe Bool)
  , _account_owner :: !(Maybe Bool)
  , _locale :: !(Maybe Text)
  , _collaborator :: !(Maybe Bool)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''AssociatedUser)

data Res = Res
  -- Offline Mode
  { _access_token :: !AccessToken
  , _scopes :: !Scopes
  -- Online Mode
  , _expired_in :: !(Maybe Word32)
  , _associated_user_scope :: !(Maybe Scopes)
  , _associated_user :: !(Maybe AssociatedUser)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Res)
