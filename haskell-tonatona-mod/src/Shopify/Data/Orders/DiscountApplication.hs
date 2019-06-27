{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.DiscountApplication where

import RIO
import Data.Aeson.TH

data DiscountApplication = DiscountApplication
  { _type :: !(Maybe Text)
  -- "type": "discount_code",
  , _value :: !(Maybe Text)
  -- "value": "10.0",
  , _value_type :: !(Maybe Text)
  -- "value_type": "percentage",
  , _allocation_method :: !(Maybe Text)
  -- "allocation_method": "across",
  , _target_selection :: !(Maybe Text)
  -- "target_selection": "all",
  , _target_type :: !(Maybe Text)
  -- "target_type": "line_item",
  , _code :: !(Maybe Text)
  -- "code": "TENOFF"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''DiscountApplication)
