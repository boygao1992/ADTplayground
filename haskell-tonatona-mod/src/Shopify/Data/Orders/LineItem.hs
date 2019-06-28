{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.LineItem where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.Property (Property)
import Shopify.Data.Orders.PriceSet (PriceSet)
import Shopify.Data.Orders.DiscountAllocation (DiscountAllocation)
import Shopify.Data.Orders.TaxLine (TaxLine)

data LineItem = LineItem
  { __id :: !(Maybe Word64)
    -- "id": 466157049,
  , __variant_id :: !(Maybe Word64)
    -- "variant_id": 39072856,
  , __title :: !(Maybe Text)
    -- "title": "IPod Nano - 8gb",
  , __quantity :: !(Maybe Word64)
    -- "quantity": 1,
  , __sku :: !(Maybe Text)
    -- "sku": "IPOD2008GREEN",
  , __variant_title :: !(Maybe Text)
    -- "variant_title": "green",
  , __vendor :: !(Maybe Text)
    -- "vendor": null,
  , __fulfillment_service :: !(Maybe Text)
    -- "fulfillment_service": "manual",
  , __product_id :: !(Maybe Word64)
    -- "product_id": 632910392,
  , __requires_shipping :: !(Maybe Bool)
    -- "requires_shipping": true,
  , __taxable :: !(Maybe Bool)
    -- "taxable": true,
  , __gift_card :: !(Maybe Bool)
    -- "gift_card": false,
  , __name :: !(Maybe Text)
    -- "name": "IPod Nano - 8gb - green",
  , __variant_inventory_management :: !(Maybe Text)
    -- "variant_inventory_management": "shopify",
  , __properties :: !(Maybe [Property])

  , __product_exists :: !(Maybe Bool)
    -- "product_exists": true,
  , __fulfillable_quantity :: !(Maybe Word64)
    -- "fulfillable_quantity": 1,
  , __grams :: !(Maybe Word64)
    -- "grams": 200,
  , __price :: !(Maybe Text)
    -- "price": "199.00",
  , __total_discount :: !(Maybe Text)
    -- "total_discount": "0.00",
  , __fulfillment_status :: !(Maybe Text)
    -- "fulfillment_status": null,

  , __price_set :: !(Maybe PriceSet)

  , __total_discount_set :: !(Maybe PriceSet)

  , __discount_allocations :: !(Maybe [DiscountAllocation])

  , __admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/LineItem/466157049",
  , __tax_lines :: !(Maybe [TaxLine])

  } deriving (Eq, Show)
$(makeLensesDropOne ''LineItem)
$(deriveJSONDropTwo ''LineItem)
