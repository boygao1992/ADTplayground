{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.Price where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

data Price = Price
  { __amount :: !(Maybe Text)
    -- "amount": "11.94",
  , __currency_code :: !(Maybe Text)
    -- "currency_code": "USD"
  } deriving (Eq, Show)
$(makeLensesDropOne ''Price)
$(deriveJSONDropTwo ''Price)
