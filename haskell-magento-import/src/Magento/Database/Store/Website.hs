{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Magento.Database.Store.Website where

import RIO

import Database.Beam.Schema

data StoreWebsiteT f = StoreWebsite
  { _website_id :: C f Word16
  , _code :: C f Text
  , _name :: C f Text
  } deriving (Generic, Beamable)
type StoreWebsite = StoreWebsiteT Identity
deriving instance Eq StoreWebsite
deriving instance Show StoreWebsite

instance Table StoreWebsiteT where
  data PrimaryKey StoreWebsiteT f = WebsiteId (C f Word16)
    deriving (Generic, Beamable)
  primaryKey = WebsiteId . _website_id
type WebsiteId = PrimaryKey StoreWebsiteT Identity
deriving instance Eq WebsiteId
deriving instance Show WebsiteId

StoreWebsite
  (LensFor website_id)
  (LensFor code)
  (LensFor name)
  = tableLenses
