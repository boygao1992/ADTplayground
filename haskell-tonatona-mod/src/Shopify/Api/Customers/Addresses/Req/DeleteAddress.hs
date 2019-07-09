{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Addresses.Req.DeleteAddress where

import RIO
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util

data Base = Base
  { __base :: ![Text]
  } deriving (Eq, Show)
$(makeLensesDropOne ''Base)
$(deriveJSONDropTwo ''Base)

data Res = Res
  { __errors :: !(Maybe Base)
  }
$(makeLensesDropOne ''Res)
$(deriveJSONDropTwo ''Res)
