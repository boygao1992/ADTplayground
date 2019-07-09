{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.DiscountAllocation where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.PriceSet (PriceSet)

data DiscountAllocation = DiscountAllocation
  { __amount :: !(Maybe Text)
    -- "amount": "5.00",
  , __discount_application_index :: !(Maybe Word64)
    -- "discount_application_index": 2,
  , __amount_set :: !(Maybe PriceSet)
  } deriving (Eq, Show)
$(makeLensesDropOne ''DiscountAllocation)
$(deriveJSONDropTwo ''DiscountAllocation)
