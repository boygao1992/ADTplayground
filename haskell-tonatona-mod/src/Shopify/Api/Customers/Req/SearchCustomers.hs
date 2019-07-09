{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.SearchCustomers where

import RIO
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util
import Data.Default

data Req = Req
  { __order :: Maybe Text
  , __query :: Maybe Text
  , __limit :: Maybe Word8
  , __fields :: Maybe Text
  }
$(makeLensesDropOne ''Req)
$(deriveJSONDropTwo ''Req)

instance Default Req where
  def = Req Nothing Nothing Nothing Nothing
