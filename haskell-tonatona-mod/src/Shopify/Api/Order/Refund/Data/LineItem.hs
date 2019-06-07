{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Refund.Data.LineItem where

import RIO
import Data.Aeson.TH
import qualified Shopify.Api.Order.Data.LineItem as Order (LineItem)
import Shopify.Api.Order.Data.PriceSet (PriceSet)

data LineItem = LineItem
  { _id :: !(Maybe Word32)
    -- "id": 209341123,
  , _line_item :: !(Maybe Order.LineItem)
    -- "line_item": {},
  , _line_item_id :: !(Maybe Word32)
    -- "line_item_id": 128323456,
  , _quantity :: !(Maybe Word32)
    -- "quantity": 2,
  , _location_id :: !(Maybe Word32)
    -- "location_id": 40642626,
  , _restock_type :: !(Maybe Text)
    -- "restock_type": "return",
  , _subtotal :: !(Maybe Word32)
    -- "subtotal": 10.99,
  , _total_tax :: !(Maybe Word32)
    -- "total_tax": 2.67,
  , _subtotal_set :: !(Maybe PriceSet)
  , _total_tax_set :: !(Maybe PriceSet)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''LineItem)
