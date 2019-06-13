module Shopify.Api.Customer.Data.Req.SearchCustomers where

import RIO

data Req = Req
  { _order :: Maybe Text
  , _query :: Maybe Text
  , _limit :: Maybe Word8
  , _fields :: Maybe Text
  }

emptyReq :: Req
emptyReq = Req Nothing Nothing Nothing Nothing
