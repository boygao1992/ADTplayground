{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Data.ShippingLine where

import RIO
import Data.Aeson.TH
import Shopify.Api.Order.Data.PriceSet (PriceSet)
import Shopify.Api.Order.Data.DiscountAllocation (DiscountAllocation)
import Shopify.Api.Order.Data.TaxLine (TaxLine)

data ShippingLine = ShippingLine
  { _id :: !(Maybe Word64)
    -- "id": 369256396,
  , _title :: !(Maybe Text)
    -- "title": "Free Shipping",
  , _price :: !(Maybe Text)
    -- "price": "0.00",
  , _code :: !(Maybe Text)
    -- "code": "Free Shipping",
  , _source :: !(Maybe Text)
    -- "source": "shopify",
  , _phone :: !(Maybe Text)
    -- "phone": null,
  , _requested_fulfillment_service_id :: !(Maybe Text)
    -- "requested_fulfillment_service_id": null,
  , _delivery_category :: !(Maybe Text)
    -- "delivery_category": null,
  , _carrier_identifier :: !(Maybe Text)
    -- "carrier_identifier": null,
  , _discounted_price :: !(Maybe Text)
    -- "discounted_price": "0.00",
  , _price_set :: !(Maybe PriceSet)
  , _discounted_price_set :: !(Maybe PriceSet)
  , _discount_allocations :: !(Maybe [DiscountAllocation])
  , _tax_lines :: !(Maybe [TaxLine])
  , _amount :: !(Maybe Text)
    -- "amount": "5.00",
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''ShippingLine)
