{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Magento.Database.Catalog.Category.Entity.Varchar where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Store as Store
import qualified Magento.Database.Catalog.Category.Entity as CatalogCategoryEntity

data CatalogCategoryEntityVarcharT f = CatalogCategoryEntityVarchar
  { _value_id :: C f Word64
  , _attribute_id :: C f Word16
  , _store_id :: PrimaryKey Store.StoreT f
  , _row_id :: PrimaryKey CatalogCategoryEntity.CatalogCategoryEntityT f
  , _value :: C f (Maybe Text)
  } deriving (Generic, Beamable)
type CatalogCategoryEntityVarchar = CatalogCategoryEntityVarcharT Identity
deriving instance Eq CatalogCategoryEntityVarchar
deriving instance Show CatalogCategoryEntityVarchar

instance Table CatalogCategoryEntityVarcharT where
  data PrimaryKey CatalogCategoryEntityVarcharT f = ValueId (C f Word64)
    deriving (Generic, Beamable)
  primaryKey = ValueId . _value_id
type ValueId = PrimaryKey CatalogCategoryEntityVarcharT Identity
deriving instance Eq ValueId
deriving instance Show ValueId

CatalogCategoryEntityVarchar
  (LensFor value_id)
  (LensFor attribute_id)
  (Store.StoreId (LensFor store_id))
  (CatalogCategoryEntity.RowId (LensFor row_id))
  (LensFor value)
  = tableLenses
