{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.Refunds.OrderAdjustment where

import RIO
import Data.Aeson.TH
import Shopify.Data.Orders.Refunds.RefundId (RefundId)
import Shopify.Data.Orders.OrderId (OrderId)
import Shopify.Data.Orders.PriceSet (PriceSet)

data OrderAdjustment = OrderAdjustment
  { _id :: !(Maybe Word64)
    -- "id": 4221763620,
  , _order_id :: !(Maybe OrderId)
    -- "order_id": 171016912932,
  , _refund_id :: !(Maybe RefundId)
    -- "refund_id": 8244756516,
  , _amount :: !(Maybe Text)
    -- "amount": "-8.00",
  , _tax_amount :: !(Maybe Text)
    -- "tax_amount": "0.00",
  , _kind :: !(Maybe Text)
    -- "kind": "shipping_refund",
  , _reason :: !(Maybe Text)
    -- "reason": "Shipping refund",
  , _amount_set :: !(Maybe PriceSet)
  , _tax_amount_set :: !(Maybe PriceSet)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''OrderAdjustment)
