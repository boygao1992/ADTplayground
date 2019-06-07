{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Data.PriceSet where

import RIO
import Data.Aeson.TH

data Money = Money
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
    ''Money)

data PriceSet = PriceSet
  { _shop_money :: !(Maybe Money)
  , _presentment_money :: !(Maybe Money)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''PriceSet)
