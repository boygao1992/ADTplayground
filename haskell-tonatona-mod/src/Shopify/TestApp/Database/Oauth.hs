{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Shopify.TestApp.Database.Oauth where

import RIO

import Database.Beam.Schema

data OauthT f = Oauth
  { _shopname :: C f Text
  , _access_token :: C f (Maybe Text)
  , _nonce :: C f (Maybe Text)
  } deriving (Generic, Beamable)
type Oauth = OauthT Identity
deriving instance Eq Oauth
deriving instance Show Oauth

instance Table OauthT where
  data PrimaryKey OauthT f = Shopname(C f Text)
    deriving (Generic, Beamable)
  primaryKey = Shopname . _shopname
type Shopname = PrimaryKey OauthT Identity
deriving instance Eq Shopname
deriving instance Show Shopname

Oauth
  (LensFor shopname)
  (LensFor access_token)
  (LensFor nonce)
  = tableLenses
