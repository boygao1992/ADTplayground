{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Magento.Database.Catalog.Category.Entity where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Eav.Attribute.Set as EavAttributeSet
import Magento.Data.CategoryPath (CategoryPath)

data CatalogCategoryEntityT f = CatalogCategoryEntity
  { _row_id :: C f Word32
  , _entity_id :: C f Word32
  , _attribute_set_id :: PrimaryKey EavAttributeSet.EavAttributeSetT f
  , _parent_id :: PrimaryKey CatalogCategoryEntityT (Nullable f) -- NOTE Recursive reference without Nullable constraint forms an infinite loop
  , _path :: C f CategoryPath
  } deriving (Generic, Beamable)
type CatalogCategoryEntity = CatalogCategoryEntityT Identity
deriving instance Eq CatalogCategoryEntity
deriving instance Show CatalogCategoryEntity

instance Table CatalogCategoryEntityT where
  data PrimaryKey CatalogCategoryEntityT f = RowId (C f Word32)
    deriving (Generic, Beamable)
  primaryKey = RowId . _row_id
type RowId = PrimaryKey CatalogCategoryEntityT Identity
deriving instance Eq RowId
deriving instance Show RowId
deriving instance Eq (PrimaryKey CatalogCategoryEntityT (Nullable Identity))
deriving instance Show (PrimaryKey CatalogCategoryEntityT (Nullable Identity))

CatalogCategoryEntity
  (LensFor row_id)
  (LensFor entity_id)
  (EavAttributeSet.AttributeSetId (LensFor attribute_set_id))
  (RowId (LensFor parent_id))
  (LensFor path)
  = tableLenses
