{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Variants.PresentmentPrice where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.Price (Price)

data PresentmentPrice = PresentmentPrice
  { __price :: !(Maybe Price)
  , __compare_at_price :: !(Maybe Text)
    -- "compare_at_price": "299.00"
  } deriving (Eq, Show)
$(makeLensesDropOne ''PresentmentPrice)
$(deriveJSONDropTwo ''PresentmentPrice)
