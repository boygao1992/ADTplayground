{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.Property where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

data Property = Property
  { __name :: !(Maybe Text)
    -- "name": "Custom Engraving Front",
  , __value :: !(Maybe Text)
    -- "value": "Happy Birthday"
  } deriving (Eq, Show)
$(makeLensesDropOne ''Property)
$(deriveJSONDropTwo ''Property)
