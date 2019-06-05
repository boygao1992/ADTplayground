{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Magento.Database.Catalog.Product.Entity where

import RIO

import Database.Beam.Schema

import Magento.Data.ProductType (ProductType)
import qualified Magento.Database.Eav.Attribute.Set as EavAttributeSet

data CatalogProductEntityT f = CatalogProductEntity
  { _row_id :: C f Word32
  , _entity_id :: C f Word32
  , _attribute_set_id :: PrimaryKey EavAttributeSet.EavAttributeSetT f
  , _type_id :: C f ProductType
  , _sku :: C f Text
  } deriving (Generic, Beamable)
type CatalogProductEntity = CatalogProductEntityT Identity
deriving instance Eq CatalogProductEntity
deriving instance Show CatalogProductEntity

instance Table CatalogProductEntityT where
  data PrimaryKey CatalogProductEntityT f = RowId (C f Word32)
    deriving (Generic, Beamable)
  primaryKey = RowId . _row_id
type RowId = PrimaryKey CatalogProductEntityT Identity
deriving instance Eq RowId
deriving instance Show RowId

CatalogProductEntity
  (LensFor row_id)
  (LensFor entity_id)
  (EavAttributeSet.AttributeSetId (LensFor attribute_set_id))
  (LensFor type_id)
  (LensFor sku)
  = tableLenses
