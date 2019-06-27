{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Variants.PresentmentPrice where

import RIO
import Data.Aeson.TH
import Shopify.Api.Order.Data.Price (Price)

data PresentmentPrice = PresentmentPrice
  { _price :: !(Maybe Price)
  , _compare_at_price :: !(Maybe Text)
    -- "compare_at_price": "299.00"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''PresentmentPrice)
