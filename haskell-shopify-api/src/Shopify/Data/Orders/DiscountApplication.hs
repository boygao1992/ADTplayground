{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.DiscountApplication where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

data DiscountApplication = DiscountApplication
  { __type :: !(Maybe Text)
  -- "type": "discount_code",
  , __value :: !(Maybe Text)
  -- "value": "10.0",
  , __value_type :: !(Maybe Text)
  -- "value_type": "percentage",
  , __allocation_method :: !(Maybe Text)
  -- "allocation_method": "across",
  , __target_selection :: !(Maybe Text)
  -- "target_selection": "all",
  , __target_type :: !(Maybe Text)
  -- "target_type": "line_item",
  , __code :: !(Maybe Text)
  -- "code": "TENOFF"
  } deriving (Eq, Show)
$(makeLensesDropOne ''DiscountApplication)
$(deriveJSONDropTwo ''DiscountApplication)
