{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Addresses.Req.GetAddresses where

import RIO
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util
import Data.Default

data Req = Req
  { __limit :: !(Maybe Word64)
  , __page :: !(Maybe Word64)
  } deriving (Eq, Show)
$(makeLensesDropOne ''Req)
$(deriveJSONDropTwo ''Req)

instance Default Req where
  def = Req Nothing Nothing
