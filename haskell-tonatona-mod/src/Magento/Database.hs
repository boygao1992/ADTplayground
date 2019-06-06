{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Magento.Database where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Eav.Entity.Type as EavEntityType
import qualified Magento.Database.Eav.Attribute as EavAttribute
import qualified Magento.Database.Store.Website as StoreWebsite
import qualified Magento.Database.Store.Group as StoreGroup
import qualified Magento.Database.Store as Store
import qualified Magento.Database.Catalog.Product.Entity.Int as CatalogProductEntityInt
import qualified Magento.Database.Eav.Attribute.Set as EavAttributeSet
import qualified Magento.Database.Eav.Attribute.Group as EavAttributeGroup
import qualified Magento.Database.Eav.Attribute.Option as EavAttributeOption
import qualified Magento.Database.Eav.Attribute.Option.Value as EavAttributeOptionValue
import qualified Magento.Database.Catalog.Product.Entity as CatalogProductEntity
import qualified Magento.Database.Catalog.Category.Entity as CatalogCategoryEntity
import qualified Magento.Database.Catalog.Category.Entity.Varchar as CatalogCategoryEntityVarchar

data MagentoDb f = MagentoDb
  { _magentoEavEntityType :: f (TableEntity EavEntityType.EavEntityTypeT)
  , _magentoEavAttribute :: f (TableEntity EavAttribute.EavAttributeT)
  , _magentoStoreWebsite :: f (TableEntity StoreWebsite.StoreWebsiteT)
  , _magentoStoreGroup :: f (TableEntity StoreGroup.StoreGroupT)
  , _magentoStore :: f (TableEntity Store.StoreT)
  , _magentoCatalogProductEntityInt :: f (TableEntity CatalogProductEntityInt.CatalogProductEntityIntT)
  , _magentoEavAttributeSet :: f (TableEntity EavAttributeSet.EavAttributeSetT)
  , _magentoEavAttributeGroup :: f (TableEntity EavAttributeGroup.EavAttributeGroupT)
  , _magentoEavAttributeOption :: f (TableEntity EavAttributeOption.EavAttributeOptionT)
  , _magentoEavAttributeOptionValue :: f (TableEntity EavAttributeOptionValue.EavAttributeOptionValueT)
  , _magentoCatalogProductEntity :: f (TableEntity CatalogProductEntity.CatalogProductEntityT)
  , _magentoCatalogCategoryEntity :: f (TableEntity CatalogCategoryEntity.CatalogCategoryEntityT)
  , _magentoCatalogCategoryEntityVarchar :: f (TableEntity CatalogCategoryEntityVarchar.CatalogCategoryEntityVarcharT)
  } deriving (Generic, Database be)

magentoDb :: DatabaseSettings be MagentoDb
magentoDb = defaultDbSettings `withDbModification`
  dbModification
  { _magentoEavAttribute = modifyTableFields tableModification
      { EavAttribute._entity_type_id = EavEntityType.EntityTypeId "entity_type_id"
      }
  , _magentoStoreGroup = modifyTableFields tableModification
      { StoreGroup._website_id = StoreWebsite.WebsiteId "website_id"
      }
  , _magentoStore = modifyTableFields tableModification
      { Store._website_id = StoreWebsite.WebsiteId "website_id"
      , Store._group_id = StoreGroup.GroupId "group_id"
      }
  , _magentoCatalogProductEntityInt = modifyTableFields tableModification
      { CatalogProductEntityInt._store_id = Store.StoreId "store_id"
      , CatalogProductEntityInt._row_id = CatalogProductEntity.RowId "row_id"
      }
  , _magentoEavAttributeSet = modifyTableFields tableModification
      { EavAttributeSet._entity_type_id = EavEntityType.EntityTypeId "entity_type_id"
      }
  , _magentoEavAttributeGroup = modifyTableFields tableModification
      { EavAttributeGroup._attribute_set_id = EavAttributeSet.AttributeSetId "attribute_set_id"
      }
  , _magentoEavAttributeOption = modifyTableFields tableModification
      { EavAttributeOption._attribute_id = EavAttribute.AttributeId "attribute_id"
      }
  , _magentoEavAttributeOptionValue = modifyTableFields tableModification
      { EavAttributeOptionValue._option_id = EavAttributeOption.OptionId "option_id"
      , EavAttributeOptionValue._store_id = Store.StoreId "store_id"
      }
  , _magentoCatalogProductEntity = modifyTableFields tableModification
      { CatalogProductEntity._attribute_set_id = EavAttributeSet.AttributeSetId "attribute_set_id"
      }
  , _magentoCatalogCategoryEntity = modifyTableFields tableModification
      { CatalogCategoryEntity._attribute_set_id = EavAttributeSet.AttributeSetId "attribute_set_id"
      , CatalogCategoryEntity._parent_id = CatalogCategoryEntity.RowId "parent_id"
      }
  , _magentoCatalogCategoryEntityVarchar = modifyTableFields tableModification
      { CatalogCategoryEntityVarchar._store_id = Store.StoreId "store_id"
      , CatalogCategoryEntityVarchar._row_id = CatalogCategoryEntity.RowId "row_id"
      }
  }

MagentoDb
  (TableLens eavEntityType)
  (TableLens eavAttribute)
  (TableLens storeWebsite)
  (TableLens storeGroup)
  (TableLens store)
  (TableLens catalogProductEntityInt)
  (TableLens eavAttributeSet)
  (TableLens eavAttributeGroup)
  (TableLens eavAttributeOption)
  (TableLens eavAttributeOptionValue)
  (TableLens catalogProductEntity)
  (TableLens catalogCategoryEntity)
  (TableLens catalogCategoryEntityVarchar)
  = dbLenses
