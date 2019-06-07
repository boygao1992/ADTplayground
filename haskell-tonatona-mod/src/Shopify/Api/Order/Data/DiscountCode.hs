{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Data.DiscountCode where

import RIO
import Data.Aeson.TH

data DiscountCode = DiscountCode
  { _code :: !(Maybe Text)
    -- "code": "TENOFF",
  , _amount :: !(Maybe Text)
    -- "amount": "10.00",
  , _type :: !(Maybe Text)
    -- "type": "percentage"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''DiscountCode)
