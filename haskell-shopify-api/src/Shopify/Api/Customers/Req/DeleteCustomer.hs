{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.DeleteCustomer where

import RIO
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util

data Res = Res
  { __errors :: !(Maybe Text)
  } deriving (Eq, Show)
$(makeLensesDropOne ''Res)
$(deriveJSONDropTwo ''Res)
