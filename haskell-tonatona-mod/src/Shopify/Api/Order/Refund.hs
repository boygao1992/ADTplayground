{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Order.Refund where

import RIO
import Data.Aeson.TH
import Shopify.Api.Order.Data.OrderId (OrderId)
import qualified Shopify.Api.Order.Refund.Data.LineItem as Refund (LineItem)
import Shopify.Api.Order.Transaction (Transaction)
import Shopify.Api.Order.Refund.Data.RefundId (RefundId)
import Shopify.Api.Order.Refund.Data.OrderAdjustment (OrderAdjustment)

data Refund = Refund
  { _id :: !(Maybe RefundId)
    -- "id": 509562969,
  , _order_id :: !(Maybe OrderId)
    -- "order_id": 450789469,
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-04-09T10:02:43-04:00",
  , _note :: !(Maybe Text)
    -- "note": "it broke during shipping",
  , _user_id :: !(Maybe Word32)
    -- "user_id": 799407056,
  , _processed_at :: !(Maybe Text)
    -- "processed_at": "2019-04-09T10:02:43-04:00",
  , _restock :: !(Maybe Bool)
    -- "restock": true,
  , _admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Refund/509562969",
  , _refund_line_items :: !(Maybe [Refund.LineItem])
  , _transactions :: !(Maybe [Transaction])
  , _order_adjustments :: !(Maybe [OrderAdjustment])
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Refund)

-- GET /admin/api/2019-04/orders/#{order_id}/refunds.json
-- Retrieves a list of refunds for an order
-- GET /admin/api/2019-04/orders/#{order_id}/refunds/#{refund_id}.json
-- Retrieves a specific refund
-- POST /admin/api/2019-04/orders/#{order_id}/refunds/calculate.json
-- Calculates a refund
-- POST /admin/api/2019-04/orders/#{order_id}/refunds.json
-- Creates a refund
