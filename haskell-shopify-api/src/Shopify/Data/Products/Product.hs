{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Products.Product where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Products.ProductId (ProductId)
import Shopify.Data.Products.Option (Option)
import Shopify.Data.Products.Variants.Variant (Variant)
import Shopify.Data.Products.Images.Image (Image)

data Product = Product
  { __id :: !(Maybe ProductId)
    -- "id": 632910392,
  , __title :: !(Maybe Text)
    -- "title": "IPod Nano - 8GB",
  , __body_html :: !(Maybe Text)
    -- "body_html": "<p>It's the small iPod with one very big idea: Video. Now the world's most popular music player, available in 4GB and 8GB models, lets you enjoy TV shows, movies, video podcasts, and more. The larger, brighter display means amazing picture quality. In six eye-catching colors, iPod nano is stunning all around. And with models starting at just $149, little speaks volumes.</p>",
  , __vendor :: !(Maybe Text)
    -- "vendor": "Apple",
  , __product_type :: !(Maybe Text)
    -- "product_type": "Cult Products",
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-05-01T15:21:27-04:00",
  , __handle :: !(Maybe Text)
    -- "handle": "ipod-nano",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2019-05-01T15:21:27-04:00",
  , __published_at :: !(Maybe Text)
    -- "published_at": "2007-12-31T19:00:00-05:00",
  , __template_suffix :: !(Maybe Text)
    -- "template_suffix": null,
  , __tags :: !(Maybe Text)
    -- "tags": "Emotive, Flash Memory, MP3, Music",
  , __published_scope :: !(Maybe Text)
    -- "published_scope": "web",
  , __admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Product/632910392",
  , __variants :: !(Maybe [Variant])
  , __options :: !(Maybe [Option])
  , __images :: !(Maybe [Image])
  , __image :: !(Maybe Image) -- NOTE deprecated?
  } deriving (Eq, Show)
$(makeLensesDropOne ''Product)
$(deriveJSONDropTwo ''Product)

data SingleProduct = SingleProduct
  { __product :: !Product
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''SingleProduct)

data Products = Products
  { __products :: ![Product]
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''Products)
