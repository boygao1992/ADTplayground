{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Option where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Products.ProductId (ProductId)

data Option = Option
  { __id :: !(Maybe Word64)
    -- "id": 594680422,
  , __product_id :: !(Maybe ProductId)
    -- "product_id": 632910392,
  , __name :: !(Maybe Text)
    -- "name": "Color",
  , __position :: !(Maybe Word32)
    -- "position": 1,
  } deriving (Eq, Show)
$(makeLensesDropOne ''Option)
$(deriveJSONDropTwo ''Option)

