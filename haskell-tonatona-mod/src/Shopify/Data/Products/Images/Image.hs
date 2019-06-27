{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Images.Image where

import RIO
import Data.Aeson.TH
import Shopify.Data.Products.ProductId (ProductId)
import Shopify.Data.Products.Images.ImageId (ImageId)
import Shopify.Data.Products.Variants.VariantId (VariantId)

data Image = Image
  { _id :: !(Maybe ImageId)
    -- "id": 850703190,
  , _product_id :: !(Maybe ProductId)
    -- "product_id": 632910392,
  , _position :: !(Maybe Word32)
    -- "position": 1,
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-05-01T15:21:27-04:00",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2019-05-01T15:21:27-04:00",
  , _alt :: !(Maybe Text)
    -- "alt": null,
  , _width :: !(Maybe Word32)
    -- "width": 123,
  , _height :: !(Maybe Word32)
    -- "height": 456,
  , _src :: !(Maybe Text)
    -- "src": "https://cdn.shopify.com/s/files/1/0006/9093/3842/products/ipod-nano.png?v=1556738487",
  , _variant_ids :: !(Maybe [VariantId])
    -- "variant_ids": [],
  , admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/ProductImage/850703190"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Image)

