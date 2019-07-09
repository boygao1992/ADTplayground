{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.CountCustomers where

import RIO
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util

data Res = Res
  { __count :: !(Maybe Word64)
  } deriving (Eq, Show)
$(makeLensesDropOne ''Res)
$(deriveJSONDropTwo ''Res)

