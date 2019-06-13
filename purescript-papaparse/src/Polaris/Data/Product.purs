module Polaris.Data.Product where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Simple.JSON (class ReadForeign, class WriteForeign)

import Polaris.Data.Product.Variant (Variant)

newtype Product = Product
  { id :: Maybe Number
    -- "id": 632910392,
  , title :: Maybe String
    -- "title": "IPod Nano - 8GB",
  -- , body_html :: Maybe String
  --   -- "body_html": "<p>It's the small iPod with one very big idea: Video. Now the world's most popular music player,  available in 4GB and 8GB models, lets you enjoy TV shows, movies, video podcasts, and more. The larger, brighter display means amazing picture quality. In six eye-catching colors, iPod nano is stunning all around. And with models starting at just $149, little speaks volumes.</p>",
  , vendor :: Maybe String
    -- "vendor": "Apple",
  , product_type :: Maybe String
    -- "product_type": "Cult Products",
  -- , created_at :: Maybe String
  --   -- "created_at": "2019-05-01T15:21:27-04:00",
  -- , handle :: Maybe String
  --   -- "handle": "ipod-nano",
  -- , updated_at :: Maybe String
  --   -- "updated_at": "2019-05-01T15:21:27-04:00",
  -- , published_at :: Maybe String
  --   -- "published_at": "2007-12-31T19:00:00-05:00",
  -- , template_suffix :: Maybe String
  --   -- "template_suffix": null,
  -- , tags :: Maybe String
  --   -- "tags": "Emotive,  Flash Memory, MP3, Music",
  -- , published_scope :: Maybe String
  --   -- "published_scope": "web",
  -- , admin_graphql_api_id :: Maybe String
  --   -- "admin_graphql_api_id": "gid://shopify/Product/632910392",
  , variants :: Maybe (Array Variant)
  -- , options :: Maybe [Option]
  -- , images :: Maybe [Image]
  -- , image :: Maybe Image -- NOTE deprecated?
  }
derive instance newtypeProduct :: Newtype Product _
derive instance genericProduct :: Generic Product _
derive newtype instance eqProduct :: Eq Product
instance showProduct :: Show Product where
  show x = genericShow x
derive newtype instance encodeJsonProduct :: EncodeJson Product
derive newtype instance deocdeJsonProduct :: DecodeJson Product
derive newtype instance readForeignProduct :: ReadForeign Product
derive newtype instance writeForeignProduct :: WriteForeign Product

newtype Products = Products
  { products :: Array Product
  }
derive instance newtypeProducts :: Newtype Products _
derive instance genericProducts :: Generic Products _
derive newtype instance eqProducts :: Eq Products
derive newtype instance showProducts :: Show Products
derive newtype instance encodeJsonProducts :: EncodeJson Products
derive newtype instance deocdeJsonProducts :: DecodeJson Products
derive newtype instance readForeignProducts :: ReadForeign Products
derive newtype instance writeForeignProducts :: WriteForeign Products
