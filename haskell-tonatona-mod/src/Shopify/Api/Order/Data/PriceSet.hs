{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Data.PriceSet where

import RIO
import Data.Aeson.TH
import Shopify.Api.Order.Data.Price (Price)

data PriceSet = PriceSet
  { _shop_money :: !(Maybe Price)
  , _presentment_money :: !(Maybe Price)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''PriceSet)
