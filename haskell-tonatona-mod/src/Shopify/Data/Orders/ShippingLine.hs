{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.ShippingLine where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.PriceSet (PriceSet)
import Shopify.Data.Orders.DiscountAllocation (DiscountAllocation)
import Shopify.Data.Orders.TaxLine (TaxLine)

data ShippingLine = ShippingLine
  { __id :: !(Maybe Word64)
    -- "id": 369256396,
  , __title :: !(Maybe Text)
    -- "title": "Free Shipping",
  , __price :: !(Maybe Text)
    -- "price": "0.00",
  , __code :: !(Maybe Text)
    -- "code": "Free Shipping",
  , __source :: !(Maybe Text)
    -- "source": "shopify",
  , __phone :: !(Maybe Text)
    -- "phone": null,
  , __requested_fulfillment_service_id :: !(Maybe Text)
    -- "requested_fulfillment_service_id": null,
  , __delivery_category :: !(Maybe Text)
    -- "delivery_category": null,
  , __carrier_identifier :: !(Maybe Text)
    -- "carrier_identifier": null,
  , __discounted_price :: !(Maybe Text)
    -- "discounted_price": "0.00",
  , __price_set :: !(Maybe PriceSet)
  , __discounted_price_set :: !(Maybe PriceSet)
  , __discount_allocations :: !(Maybe [DiscountAllocation])
  , __tax_lines :: !(Maybe [TaxLine])
  , __amount :: !(Maybe Text)
    -- "amount": "5.00",
  } deriving (Eq, Show)
$(makeLensesDropOne ''ShippingLine)
$(deriveJSONDropTwo ''ShippingLine)
