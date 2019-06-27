{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.TaxLine where

import RIO
import Data.Aeson.TH
import Shopify.Data.Orders.PriceSet (PriceSet)

data TaxLine = TaxLine
  { _price :: !(Maybe Text)
    -- "price": "11.94",
  , _rate :: !(Maybe Double)
    -- "rate": 0.06,
  , _title :: !(Maybe Text)
    -- "title": "State Tax",
  , _price_set :: !(Maybe PriceSet)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''TaxLine)
