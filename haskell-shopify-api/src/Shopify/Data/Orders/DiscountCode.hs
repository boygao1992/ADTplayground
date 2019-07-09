{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.DiscountCode where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

data DiscountCode = DiscountCode
  { __code :: !(Maybe Text)
    -- "code": "TENOFF",
  , __amount :: !(Maybe Text)
    -- "amount": "10.00",
  , __type :: !(Maybe Text)
    -- "type": "percentage"
  } deriving (Eq, Show)
$(makeLensesDropOne ''DiscountCode)
$(deriveJSONDropTwo ''DiscountCode)
