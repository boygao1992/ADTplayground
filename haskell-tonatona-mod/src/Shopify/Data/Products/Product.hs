{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Product where

import RIO
import Data.Aeson.TH
import Shopify.Data.Products.ProductId (ProductId)
import Shopify.Data.Products.Option (Option)
import Shopify.Data.Products.Variants.Variant (Variant)
import Shopify.Data.Products.Images.Image (Image)

data Product = Product
  { _id :: !(Maybe ProductId)
    -- "id": 632910392,
  , _title :: !(Maybe Text)
    -- "title": "IPod Nano - 8GB",
  , _body_html :: !(Maybe Text)
    -- "body_html": "<p>It's the small iPod with one very big idea: Video. Now the world's most popular music player, available in 4GB and 8GB models, lets you enjoy TV shows, movies, video podcasts, and more. The larger, brighter display means amazing picture quality. In six eye-catching colors, iPod nano is stunning all around. And with models starting at just $149, little speaks volumes.</p>",
  , _vendor :: !(Maybe Text)
    -- "vendor": "Apple",
  , _product_type :: !(Maybe Text)
    -- "product_type": "Cult Products",
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-05-01T15:21:27-04:00",
  , _handle :: !(Maybe Text)
    -- "handle": "ipod-nano",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2019-05-01T15:21:27-04:00",
  , _published_at :: !(Maybe Text)
    -- "published_at": "2007-12-31T19:00:00-05:00",
  , _template_suffix :: !(Maybe Text)
    -- "template_suffix": null,
  , _tags :: !(Maybe Text)
    -- "tags": "Emotive, Flash Memory, MP3, Music",
  , _published_scope :: !(Maybe Text)
    -- "published_scope": "web",
  , _admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Product/632910392",
  , _variants :: !(Maybe [Variant])
  , _options :: !(Maybe [Option])
  , _images :: !(Maybe [Image])
  , _image :: !(Maybe Image) -- NOTE deprecated?
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Product)

data SingleProduct = SingleProduct
  { _product :: !Product
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''SingleProduct)

data Products = Products
  { _products :: ![Product]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Products)
