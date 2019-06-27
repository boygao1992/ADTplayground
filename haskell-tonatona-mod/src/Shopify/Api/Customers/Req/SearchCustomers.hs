{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.SearchCustomers where

import RIO
import Lens.Micro.TH
import Data.Default

data Req = Req
  { _order :: Maybe Text
  , _query :: Maybe Text
  , _limit :: Maybe Word8
  , _fields :: Maybe Text
  }
$(makeLenses ''Req)

instance Default Req where
  def = Req Nothing Nothing Nothing Nothing
