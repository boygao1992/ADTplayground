{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.Refunds.LineItem where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import qualified Shopify.Data.Orders.LineItem as Order (LineItem)
import Shopify.Data.Orders.PriceSet (PriceSet)

data LineItem = LineItem
  { __id :: !(Maybe Word64)
    -- "id": 209341123,
  , __line_item :: !(Maybe Order.LineItem)
    -- "line_item": {},
  , __line_item_id :: !(Maybe Word64)
    -- "line_item_id": 128323456,
  , __quantity :: !(Maybe Word64)
    -- "quantity": 2,
  , __location_id :: !(Maybe Word64)
    -- "location_id": 40642626,
  , __restock_type :: !(Maybe Text)
    -- "restock_type": "return",
  , __subtotal :: !(Maybe Word64)
    -- "subtotal": 10.99,
  , __total_tax :: !(Maybe Word64)
    -- "total_tax": 2.67,
  , __subtotal_set :: !(Maybe PriceSet)
  , __total_tax_set :: !(Maybe PriceSet)
  } deriving (Eq, Show)
$(makeLensesDropOne ''LineItem)
$(deriveJSONDropTwo ''LineItem)
