{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Magento.Database.Store where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Store.Website as StoreWebsite
import qualified Magento.Database.Store.Group as StoreGroup

data StoreT f = Store
  { _store_id :: C f Word16
  , _code :: C f Text
  , _website_id :: PrimaryKey StoreWebsite.StoreWebsiteT f
  , _group_id :: PrimaryKey StoreGroup.StoreGroupT f
  , _name :: C f Text
  } deriving (Generic, Beamable)
type Store = StoreT Identity
deriving instance Eq Store
deriving instance Show Store

instance Table StoreT where
  data PrimaryKey StoreT f = StoreId (C f Word16)
    deriving (Generic, Beamable)
  primaryKey = StoreId . _store_id
type StoreId = PrimaryKey StoreT Identity
deriving instance Eq StoreId
deriving instance Show StoreId

Store
  (LensFor store_id)
  (LensFor code)
  (StoreWebsite.WebsiteId (LensFor website_))
  (StoreGroup.GroupId (LensFor group_id))
  (LensFor name)
  = tableLenses
