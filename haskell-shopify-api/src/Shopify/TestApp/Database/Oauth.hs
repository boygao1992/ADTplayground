{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Shopify.TestApp.Database.Oauth where

import RIO

import Database.Beam.Schema
import Shopify.Api.Admin.OAuth.Data.AccessToken (AccessToken)

data OauthT f = Oauth
  { _shopname :: C f Text
  , _access_token :: C f (Maybe AccessToken)
  , _nonce :: C f (Maybe Text)
  } deriving (Generic, Beamable)
type Oauth = OauthT Identity
deriving instance Eq Oauth
deriving instance Show Oauth

instance Table OauthT where
  data PrimaryKey OauthT f = Shopname (C f Text)
    deriving (Generic, Beamable)
  primaryKey = Shopname . _shopname
type Shopname = PrimaryKey OauthT Identity
deriving instance Eq Shopname
deriving instance Show Shopname

Oauth
  (LensFor oauthShopname)
  (LensFor oauthAccessToken)
  (LensFor oauthNonce)
  = tableLenses
