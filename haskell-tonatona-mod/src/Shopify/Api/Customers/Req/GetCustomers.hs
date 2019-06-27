{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.GetCustomers where

import RIO
import Data.Default
import Lens.Micro.TH

import Shopify.Data.Customers.CustomerId (CustomerId)

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
$(makeLenses ''Req)

instance Default Req where
  def = Req Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
