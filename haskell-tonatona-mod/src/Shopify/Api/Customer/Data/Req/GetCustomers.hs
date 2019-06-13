module Shopify.Api.Customer.Data.Req.GetCustomers where

import RIO

import Shopify.Api.Customer.Data.CustomerId (CustomerId)

data Req = Req
  { _ids :: Maybe Text
  , _since_id :: Maybe CustomerId
  , _created_at_min :: Maybe Text
  , _created_at_max :: Maybe Text
  , _updated_at_min :: Maybe Text
  , _updated_at_max :: Maybe Text
  , _limit :: Maybe Word8
  , _fields :: Maybe Text
  }

emptyReq :: Req
emptyReq = Req Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
