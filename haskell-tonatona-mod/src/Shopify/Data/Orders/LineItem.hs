{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.LineItem where

import RIO
import Data.Aeson.TH
import Shopify.Data.Orders.PriceSet (PriceSet)
import Shopify.Data.Orders.DiscountAllocation (DiscountAllocation)
import Shopify.Data.Orders.TaxLine (TaxLine)

data Property = Property
  { __name :: !(Maybe Text)
    -- "name": "Custom Engraving Front",
  , __value :: !(Maybe Text)
    -- "value": "Happy Birthday"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 2
      , omitNothingFields = True
      }
    ''Property)


data LineItem = LineItem
  { _id :: !(Maybe Word64)
    -- "id": 466157049,
  , _variant_id :: !(Maybe Word64)
    -- "variant_id": 39072856,
  , _title :: !(Maybe Text)
    -- "title": "IPod Nano - 8gb",
  , _quantity :: !(Maybe Word64)
    -- "quantity": 1,
  , _sku :: !(Maybe Text)
    -- "sku": "IPOD2008GREEN",
  , _variant_title :: !(Maybe Text)
    -- "variant_title": "green",
  , _vendor :: !(Maybe Text)
    -- "vendor": null,
  , _fulfillment_service :: !(Maybe Text)
    -- "fulfillment_service": "manual",
  , _product_id :: !(Maybe Word64)
    -- "product_id": 632910392,
  , _requires_shipping :: !(Maybe Bool)
    -- "requires_shipping": true,
  , _taxable :: !(Maybe Bool)
    -- "taxable": true,
  , _gift_card :: !(Maybe Bool)
    -- "gift_card": false,
  , _name :: !(Maybe Text)
    -- "name": "IPod Nano - 8gb - green",
  , _variant_inventory_management :: !(Maybe Text)
    -- "variant_inventory_management": "shopify",
  , _properties :: !(Maybe [Property])

  , _product_exists :: !(Maybe Bool)
    -- "product_exists": true,
  , _fulfillable_quantity :: !(Maybe Word64)
    -- "fulfillable_quantity": 1,
  , _grams :: !(Maybe Word64)
    -- "grams": 200,
  , _price :: !(Maybe Text)
    -- "price": "199.00",
  , _total_discount :: !(Maybe Text)
    -- "total_discount": "0.00",
  , _fulfillment_status :: !(Maybe Text)
    -- "fulfillment_status": null,

  , _price_set :: !(Maybe PriceSet)

  , _total_discount_set :: !(Maybe PriceSet)

  , _discount_allocations :: !(Maybe [DiscountAllocation])

  , _admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/LineItem/466157049",
  , _tax_lines :: !(Maybe [TaxLine])

  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''LineItem)
