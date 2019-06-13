{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Products.Data.Option where

import RIO
import Data.Aeson.TH
import Shopify.Api.Products.Data.ProductId (ProductId)

data Option = Option
  { _id :: !(Maybe Word64)
    -- "id": 594680422,
  , _product_id :: !(Maybe ProductId)
    -- "product_id": 632910392,
  , _name :: !(Maybe Text)
    -- "name": "Color",
  , _position :: !(Maybe Word32)
    -- "position": 1,
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Option)

