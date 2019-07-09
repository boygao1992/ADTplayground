{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.Refunds.OrderAdjustment where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.Refunds.RefundId (RefundId)
import Shopify.Data.Orders.OrderId (OrderId)
import Shopify.Data.Orders.PriceSet (PriceSet)

data OrderAdjustment = OrderAdjustment
  { __id :: !(Maybe Word64)
    -- "id": 4221763620,
  , __order_id :: !(Maybe OrderId)
    -- "order_id": 171016912932,
  , __refund_id :: !(Maybe RefundId)
    -- "refund_id": 8244756516,
  , __amount :: !(Maybe Text)
    -- "amount": "-8.00",
  , __tax_amount :: !(Maybe Text)
    -- "tax_amount": "0.00",
  , __kind :: !(Maybe Text)
    -- "kind": "shipping_refund",
  , __reason :: !(Maybe Text)
    -- "reason": "Shipping refund",
  , __amount_set :: !(Maybe PriceSet)
  , __tax_amount_set :: !(Maybe PriceSet)
  } deriving (Eq, Show)
$(makeLensesDropOne ''OrderAdjustment)
$(deriveJSONDropTwo ''OrderAdjustment)
