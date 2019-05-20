{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
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
      }
  , _magentoEavAttributeSet = modifyTableFields tableModification
      { EavAttributeSet._entity_type_id = EavEntityType.EntityTypeId "entity_type_id"
      }
  , _magentoEavAttributeGroup = modifyTableFields tableModification
      { EavAttributeGroup._attribute_set_id = EavAttributeSet.AttributeSetId "attribute_set_id"
      }
  , _magentoEavAttributeOptionValue = modifyTableFields tableModification
      { EavAttributeOptionValue._option_id = EavAttributeOption.OptionId "option_id"
      , EavAttributeOptionValue._store_id = Store.StoreId "store_id"
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
  = dbLenses
