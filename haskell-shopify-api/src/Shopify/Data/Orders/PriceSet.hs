{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.PriceSet where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.Price (Price)

data PriceSet = PriceSet
  { __shop_money :: !(Maybe Price)
  , __presentment_money :: !(Maybe Price)
  } deriving (Eq, Show)
$(makeLensesDropOne ''PriceSet)
$(deriveJSONDropTwo ''PriceSet)
