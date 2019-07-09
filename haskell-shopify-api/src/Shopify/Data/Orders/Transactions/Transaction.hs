{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Orders.Transactions.Transaction where

import RIO
import Data.Aeson (Value)
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.Transactions.ErrorCode (ErrorCode)
import Shopify.Data.Orders.Transactions.CurrencyExchangeAdjustment (CurrencyExchangeAdjustment)
import Shopify.Data.Orders.Transactions.PaymentDetail (PaymentDetail)
import Shopify.Data.Orders.Transactions.TransactionId (TransactionId)

data Transaction = Transaction
  { __id :: !(Maybe TransactionId)
      -- "id": 389404469,
  , __order_id :: !(Maybe TransactionId)
      -- "order_id": 450789469,
  , __kind :: !(Maybe Text)
      -- "kind": "authorization",
  , __gateway :: !(Maybe Text)
      -- "gateway": "bogus",
  , __status :: !(Maybe Text)
      -- "status": "success",
  , __message :: !(Maybe Text)
      -- "message": null,
  , __created_at :: !(Maybe Text)
      -- "created_at": "2005-08-01T11:57:11-04:00",
  , __test :: !(Maybe Bool)
      -- "test": false,
  , __authorization :: !(Maybe Text)
      -- "authorization": "authorization-key",
  , __location_id :: !(Maybe Word64)
      -- "location_id": null,
  , __user_id :: !(Maybe Word64)
      -- "user_id": null,
  , __parent_id :: !(Maybe Word64)
      -- "parent_id": null,
  , __processed_at :: !(Maybe Text)
      -- "processed_at": "2005-08-01T11:57:11-04:00",
  , __device_id :: !(Maybe Word64)
      -- "device_id": null,
  , __receipt :: !(Maybe Value)
    -- NOTE The value of this field depends on which gateway the shop is using.
      -- "receipt": {
      --   "testcase": true,
      --   "authorization": "123456"
      -- },
  , __error_code :: !(Maybe ErrorCode)
      -- "error_code": null,
  , __source_name :: !(Maybe Text)
      -- "source_name": "web",
  , __currency_exchange_adjustment :: !(Maybe CurrencyExchangeAdjustment)
      -- "currency_exchange_adjustment": null,
  , __amount :: !(Maybe Text)
      -- "amount": "409.94",
  , __currency :: !(Maybe Text)
      -- "currency": "USD",
  , __admin_graphql_api_id :: !(Maybe Text)
      -- "admin_graphql_api_id": "gid://shopify/OrderTransaction/389404469",
  , __payment_details :: !(Maybe PaymentDetail)
  } deriving (Eq, Show)
$(makeLensesDropOne ''Transaction)
$(deriveJSONDropTwo ''Transaction)

-- GET /admin/api/2019-04/orders/#{order_id}/transactions.json
-- Retrieves a list of transactions
-- GET /admin/api/2019-04/orders/#{order_id}/transactions/count.json
-- Retrieves a count of an order's transactions
-- GET /admin/api/2019-04/orders/#{order_id}/transactions/#{transaction_id}.json
-- Retrieves a specific transaction
-- POST /admin/api/2019-04/orders/#{order_id}/transactions.json
-- Creates a transaction for an order
