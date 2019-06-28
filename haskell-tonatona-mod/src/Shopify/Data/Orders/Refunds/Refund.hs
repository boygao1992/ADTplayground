{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Orders.Refunds.Refund where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.OrderId (OrderId)
import Shopify.Data.Orders.Refunds.LineItem as Refund (LineItem)
import Shopify.Data.Orders.Transactions.Transaction (Transaction)
import Shopify.Data.Orders.Refunds.RefundId (RefundId)
import Shopify.Data.Orders.Refunds.OrderAdjustment (OrderAdjustment)

data Refund = Refund
  { __id :: !(Maybe RefundId)
    -- "id": 509562969,
  , __order_id :: !(Maybe OrderId)
    -- "order_id": 450789469,
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-04-09T10:02:43-04:00",
  , __note :: !(Maybe Text)
    -- "note": "it broke during shipping",
  , __user_id :: !(Maybe Word64)
    -- "user_id": 799407056,
  , __processed_at :: !(Maybe Text)
    -- "processed_at": "2019-04-09T10:02:43-04:00",
  , __restock :: !(Maybe Bool)
    -- "restock": true,
  , __admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Refund/509562969",
  , __refund_line_items :: !(Maybe [Refund.LineItem])
  , __transactions :: !(Maybe [Transaction])
  , __order_adjustments :: !(Maybe [OrderAdjustment])
  } deriving (Eq, Show)
$(makeLensesDropOne ''Refund)
$(deriveJSONDropTwo ''Refund)

-- GET /admin/api/2019-04/orders/#{order_id}/refunds.json
-- Retrieves a list of refunds for an order
-- GET /admin/api/2019-04/orders/#{order_id}/refunds/#{refund_id}.json
-- Retrieves a specific refund
-- POST /admin/api/2019-04/orders/#{order_id}/refunds/calculate.json
-- Calculates a refund
-- POST /admin/api/2019-04/orders/#{order_id}/refunds.json
-- Creates a refund
