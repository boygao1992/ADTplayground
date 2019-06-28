{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.TaxLine where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.PriceSet (PriceSet)

data TaxLine = TaxLine
  { __price :: !(Maybe Text)
    -- "price": "11.94",
  , __rate :: !(Maybe Double)
    -- "rate": 0.06,
  , __title :: !(Maybe Text)
    -- "title": "State Tax",
  , __price_set :: !(Maybe PriceSet)
  } deriving (Eq, Show)
$(makeLensesDropOne ''TaxLine)
$(deriveJSONDropTwo ''TaxLine)
