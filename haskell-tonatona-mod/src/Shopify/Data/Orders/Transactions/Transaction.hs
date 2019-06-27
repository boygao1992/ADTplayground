{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Orders.Transactions.Transaction where

import RIO
import Data.Aeson (Value)
import Data.Aeson.TH
import Shopify.Data.Orders.Transactions.ErrorCode (ErrorCode)
import Shopify.Data.Orders.Transactions.CurrencyExchangeAdjustment (CurrencyExchangeAdjustment)
import Shopify.Data.Orders.Transactions.PaymentDetail (PaymentDetail)
import Shopify.Data.Orders.Transactions.TransactionId (TransactionId)

data Transaction = Transaction
  { _id :: !(Maybe TransactionId)
      -- "id": 389404469,
  , _order_id :: !(Maybe TransactionId)
      -- "order_id": 450789469,
  , _kind :: !(Maybe Text)
      -- "kind": "authorization",
  , _gateway :: !(Maybe Text)
      -- "gateway": "bogus",
  , _status :: !(Maybe Text)
      -- "status": "success",
  , _message :: !(Maybe Text)
      -- "message": null,
  , _created_at :: !(Maybe Text)
      -- "created_at": "2005-08-01T11:57:11-04:00",
  , _test :: !(Maybe Bool)
      -- "test": false,
  , _authorization :: !(Maybe Text)
      -- "authorization": "authorization-key",
  , _location_id :: !(Maybe Word64)
      -- "location_id": null,
  , _user_id :: !(Maybe Word64)
      -- "user_id": null,
  , _parent_id :: !(Maybe Word64)
      -- "parent_id": null,
  , _processed_at :: !(Maybe Text)
      -- "processed_at": "2005-08-01T11:57:11-04:00",
  , _device_id :: !(Maybe Word64)
      -- "device_id": null,
  , _receipt :: !(Maybe Value)
    -- NOTE The value of this field depends on which gateway the shop is using.
      -- "receipt": {
      --   "testcase": true,
      --   "authorization": "123456"
      -- },
  , _error_code :: !(Maybe ErrorCode)
      -- "error_code": null,
  , _source_name :: !(Maybe Text)
      -- "source_name": "web",
  , _currency_exchange_adjustment :: !(Maybe CurrencyExchangeAdjustment)
      -- "currency_exchange_adjustment": null,
  , _amount :: !(Maybe Text)
      -- "amount": "409.94",
  , _currency :: !(Maybe Text)
      -- "currency": "USD",
  , _admin_graphql_api_id :: !(Maybe Text)
      -- "admin_graphql_api_id": "gid://shopify/OrderTransaction/389404469",
  , _payment_details :: !(Maybe PaymentDetail)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Transaction)

-- GET /admin/api/2019-04/orders/#{order_id}/transactions.json
-- Retrieves a list of transactions
-- GET /admin/api/2019-04/orders/#{order_id}/transactions/count.json
-- Retrieves a count of an order's transactions
-- GET /admin/api/2019-04/orders/#{order_id}/transactions/#{transaction_id}.json
-- Retrieves a specific transaction
-- POST /admin/api/2019-04/orders/#{order_id}/transactions.json
-- Creates a transaction for an order
