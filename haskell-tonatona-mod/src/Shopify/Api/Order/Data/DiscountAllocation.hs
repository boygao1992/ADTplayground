{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Data.DiscountAllocation where

import RIO
import Data.Aeson.TH
import Shopify.Api.Order.Data.PriceSet (PriceSet)

data DiscountAllocation = DiscountAllocation
  { _amount :: !(Maybe Text)
    -- "amount": "5.00",
  , _discount_application_index :: !(Maybe Word64)
    -- "discount_application_index": 2,
  , _amount_set :: !(Maybe PriceSet)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''DiscountAllocation)
