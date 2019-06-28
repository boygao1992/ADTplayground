{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Images.Image where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Products.ProductId (ProductId)
import Shopify.Data.Products.Images.ImageId (ImageId)
import Shopify.Data.Products.Variants.VariantId (VariantId)

data Image = Image
  { __id :: !(Maybe ImageId)
    -- "id": 850703190,
  , __product_id :: !(Maybe ProductId)
    -- "product_id": 632910392,
  , __position :: !(Maybe Word32)
    -- "position": 1,
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-05-01T15:21:27-04:00",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2019-05-01T15:21:27-04:00",
  , __alt :: !(Maybe Text)
    -- "alt": null,
  , __width :: !(Maybe Word32)
    -- "width": 123,
  , __height :: !(Maybe Word32)
    -- "height": 456,
  , __src :: !(Maybe Text)
    -- "src": "https://cdn.shopify.com/s/files/1/0006/9093/3842/products/ipod-nano.png?v=1556738487",
  , __variant_ids :: !(Maybe [VariantId])
    -- "variant_ids": [],
  , admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/ProductImage/850703190"
  } deriving (Eq, Show)
$(makeLensesDropOne ''Image)
$(deriveJSONDropTwo ''Image)

