{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Magento.Database.Catalog.Product.Entity.Int where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Store as Store
import qualified Magento.Database.Catalog.Product.Entity as CatalogProductEntity

data CatalogProductEntityIntT f = CatalogProductEntityInt
  { _value_id :: C f Word64
  , _attribute_id :: C f Word16
  , _store_id :: PrimaryKey Store.StoreT f
  , _row_id :: PrimaryKey CatalogProductEntity.CatalogProductEntityT f
  , _value :: C f (Maybe Int64)
  } deriving (Generic, Beamable)
type CatalogProductEntityInt = CatalogProductEntityIntT Identity
deriving instance Eq CatalogProductEntityInt
deriving instance Show CatalogProductEntityInt

instance Table CatalogProductEntityIntT where
  data PrimaryKey CatalogProductEntityIntT f = ValueId (C f Word64)
    deriving (Generic, Beamable)
  primaryKey = ValueId . _value_id
type ValueId = PrimaryKey CatalogProductEntityIntT Identity
deriving instance Eq ValueId
deriving instance Show ValueId

CatalogProductEntityInt
  (LensFor value_id)
  (LensFor attribute_id)
  (Store.StoreId (LensFor store_id))
  (CatalogProductEntity.RowId (LensFor row_id))
  (LensFor value)
  = tableLenses
