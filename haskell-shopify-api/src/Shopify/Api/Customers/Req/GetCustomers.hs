{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.GetCustomers where

import RIO
import Data.Default
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util

import Shopify.Data.Customers.CustomerId (CustomerId)

data Req = Req
  { __ids :: Maybe Text
  , __since_id :: Maybe CustomerId
  , __created_at_min :: Maybe Text
  , __created_at_max :: Maybe Text
  , __updated_at_min :: Maybe Text
  , __updated_at_max :: Maybe Text
  , __limit :: Maybe Word8
  , __fields :: Maybe Text
  }
$(makeLensesDropOne ''Req)
$(deriveJSONDropTwo ''Req)

instance Default Req where
  def = Req Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
