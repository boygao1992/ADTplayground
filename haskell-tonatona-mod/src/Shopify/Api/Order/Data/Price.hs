{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Data.Price where

import RIO
import Data.Aeson.TH

data Price = Price
  { _amount :: !(Maybe Text)
    -- "amount": "11.94",
  , _currency_code :: !(Maybe Text)
    -- "currency_code": "USD"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Price)
