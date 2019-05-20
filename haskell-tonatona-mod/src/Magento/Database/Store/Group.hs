{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Magento.Database.Store.Group where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Store.Website as StoreWebsite

data StoreGroupT f = StoreGroup
  { _group_id :: C f Word16
  , _website_id :: PrimaryKey StoreWebsite.StoreWebsiteT f
  , _code :: C f Text
  , _name :: C f Text
  , _root_category_id :: C f Word32
  } deriving (Generic, Beamable)
type StoreGroup = StoreGroupT Identity
deriving instance Eq StoreGroup
deriving instance Show StoreGroup

instance Table StoreGroupT where
  data PrimaryKey StoreGroupT f = GroupId (C f Word16)
    deriving (Generic, Beamable)
  primaryKey = GroupId . _group_id
type GroupId = PrimaryKey StoreGroupT Identity
deriving instance Eq GroupId
deriving instance Show GroupId

StoreGroup
  (LensFor group_id)
  (StoreWebsite.WebsiteId (LensFor website_id))
  (LensFor code)
  (LensFor name)
  (LensFor root_category_id)
  = tableLenses
