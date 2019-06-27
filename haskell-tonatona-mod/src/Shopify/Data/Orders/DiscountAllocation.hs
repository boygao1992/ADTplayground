{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.DiscountAllocation where

import RIO
import Data.Aeson.TH
import Shopify.Data.Orders.PriceSet (PriceSet)

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
