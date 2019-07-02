module Polaris.Data.Product.Variant where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Variant = Variant
  { id :: Maybe Number
    -- "id": 808950810,
  , product_id :: Maybe Number
    -- "product_id": 632910392,
  , title :: Maybe String
    -- "title": "Pink",
  , price :: Maybe String
    -- "price": "199.00",
  , sku :: Maybe String
    -- "sku": "IPOD2008PINK",
  -- , position :: Maybe Int
  --   -- "position": 1,
  -- , inventory_policy :: Maybe String
  --   -- "inventory_policy": "continue",
  -- , compare_at_price :: Maybe String
  --   -- "compare_at_price": null,
  -- , fulfillment_service :: Maybe String
  --   -- "fulfillment_service": "manual",
  -- , inventory_management :: Maybe String
  --   -- "inventory_management": "shopify",
  -- , option1 :: Maybe String
  --   -- "option1": "Pink",
  -- , option2 :: Maybe String
  --   -- "option2": null,
  -- , option3 :: Maybe String
  --   -- "option3": null,
  }

derive instance newtypeVariant :: Newtype Variant _
derive instance genericVariant :: Generic Variant _
derive newtype instance eqVariant :: Eq Variant
instance showVariant :: Show Variant where
  show x = genericShow x
derive newtype instance encodeJsonVariant :: EncodeJson Variant
derive newtype instance deocdeJsonVariant :: DecodeJson Variant
derive newtype instance readForeignVariant :: ReadForeign Variant
derive newtype instance writeForeignVariant :: WriteForeign Variant
