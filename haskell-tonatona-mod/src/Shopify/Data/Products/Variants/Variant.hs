{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Variants.Variant where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Products.Variants.VariantId (VariantId)
import Shopify.Data.Products.ProductId (ProductId)
import Shopify.Data.InventoryItems.InventoryItemId (InventoryItemId)
import Shopify.Data.Products.Variants.PresentmentPrice (PresentmentPrice)

data Variant = Variant
  { __id :: !(Maybe VariantId)
    -- "id": 808950810,
  , __product_id :: !(Maybe ProductId)
    -- "product_id": 632910392,
  , __title :: !(Maybe Text)
    -- "title": "Pink",
  , __price :: !(Maybe Text)
    -- "price": "199.00",
  , __sku :: !(Maybe Text)
    -- "sku": "IPOD2008PINK",
  , __position :: !(Maybe Int)
    -- "position": 1,
  , __inventory_policy :: !(Maybe Text)
    -- "inventory_policy": "continue",
  , __compare_at_price :: !(Maybe Text)
    -- "compare_at_price": null,
  , __fulfillment_service :: !(Maybe Text)
    -- "fulfillment_service": "manual",
  , __inventory_management :: !(Maybe Text)
    -- "inventory_management": "shopify",
  , __option1 :: !(Maybe Text)
    -- "option1": "Pink",
  , __option2 :: !(Maybe Text)
    -- "option2": null,
  , __option3 :: !(Maybe Text)
    -- "option3": null,
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-05-09T13:28:36-04:00",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2019-05-09T13:28:36-04:00",
  , __taxable :: !(Maybe Bool)
    -- "taxable": true,
  , __barcode :: !(Maybe Text)
    -- "barcode": "1234_pink",
  , __grams :: !(Maybe Double)
    -- "grams": 567,
  , __image_id :: !(Maybe Word64)
    -- "image_id": 562641783,
  , __weight :: !(Maybe Double)
    -- "weight": 1.25,
  , __weight_unit :: !(Maybe Text)
    -- "weight_unit": "lb",
  , __inventory_item_id :: !(Maybe InventoryItemId)
    -- "inventory_item_id": 808950810,
  , __inventory_quantity :: !(Maybe Word64)
    -- "inventory_quantity": 10,
  , __requires_shipping :: !(Maybe Bool)
    -- "requires_shipping": true,
  , __admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/ProductVariant/808950810",
  , __presentment_prices :: !(Maybe [PresentmentPrice])
  } deriving (Eq, Show)
$(makeLensesDropOne ''Variant)
$(deriveJSONDropTwo ''Variant)

data SingleVariant = SingleVariant
  { __variant :: !Variant
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''SingleVariant)

data Variants = Variants
  { __variants :: ![Variant]
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''Variants)
