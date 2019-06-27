{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Variants.Variant where

import RIO
import Data.Aeson.TH
import Shopify.Data.Products.Variants.VariantId (VariantId)
import Shopify.Data.Products.ProductId (ProductId)
import Shopify.Data.InventoryItems.InventoryItemId (InventoryItemId)
import Shopify.Data.Products.Variants.PresentmentPrice (PresentmentPrice)

data Variant = Variant
  { _id :: !(Maybe VariantId)
    -- "id": 808950810,
  , _product_id :: !(Maybe ProductId)
    -- "product_id": 632910392,
  , _title :: !(Maybe Text)
    -- "title": "Pink",
  , _price :: !(Maybe Text)
    -- "price": "199.00",
  , _sku :: !(Maybe Text)
    -- "sku": "IPOD2008PINK",
  , _position :: !(Maybe Int)
    -- "position": 1,
  , _inventory_policy :: !(Maybe Text)
    -- "inventory_policy": "continue",
  , _compare_at_price :: !(Maybe Text)
    -- "compare_at_price": null,
  , _fulfillment_service :: !(Maybe Text)
    -- "fulfillment_service": "manual",
  , _inventory_management :: !(Maybe Text)
    -- "inventory_management": "shopify",
  , _option1 :: !(Maybe Text)
    -- "option1": "Pink",
  , _option2 :: !(Maybe Text)
    -- "option2": null,
  , _option3 :: !(Maybe Text)
    -- "option3": null,
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-05-09T13:28:36-04:00",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2019-05-09T13:28:36-04:00",
  , _taxable :: !(Maybe Bool)
    -- "taxable": true,
  , _barcode :: !(Maybe Text)
    -- "barcode": "1234_pink",
  , _grams :: !(Maybe Double)
    -- "grams": 567,
  , _image_id :: !(Maybe Word64)
    -- "image_id": 562641783,
  , _weight :: !(Maybe Double)
    -- "weight": 1.25,
  , _weight_unit :: !(Maybe Text)
    -- "weight_unit": "lb",
  , _inventory_item_id :: !(Maybe InventoryItemId)
    -- "inventory_item_id": 808950810,
  , _inventory_quantity :: !(Maybe Word64)
    -- "inventory_quantity": 10,
  , _requires_shipping :: !(Maybe Bool)
    -- "requires_shipping": true,
  , _admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/ProductVariant/808950810",
  , _presentment_prices :: !(Maybe [PresentmentPrice])
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Variant)

data SingleVariant = SingleVariant
  { _variant :: !Variant
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''SingleVariant)

data Variants = Variants
  { _variants :: ![Variant]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Variants)
