module Magento.Import.Data.Categories where

import Prelude

import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (note)
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.String.Read (class Read, read)
import Data.Traversable (traverse)

type VerifiedCategory
  = { category :: Category
    , validity :: Boolean
    }

stampValidity :: Categories -> Categories -> Array VerifiedCategory
stampValidity (Categories invalid) (Categories raw)
  = (\c -> { category: c, validity: Array.notElem c invalid }) <$> raw

columnName = "categories" :: String

newtype Category = Category (Array String)
derive newtype instance eqCategory :: Eq Category
derive instance newtypeCategory :: Newtype Category _
derive newtype instance semigroupCategory :: Semigroup Category
derive newtype instance monoidCategory :: Monoid Category
instance readCategory :: Read Category where
  read = Just <<< Category <<< map String.trim <<< String.split (String.Pattern "/")
instance showCategory :: Show Category where
  show = Array.intercalate "/" <<< unwrap
instance encodeJsonCategory :: EncodeJson Category where
  encodeJson = encodeJson <<< show
instance decodeJsonCategory :: DecodeJson Category where
  decodeJson = note "" <<< read <=< decodeJson

newtype Categories = Categories (Array Category)
derive newtype instance eqCategories :: Eq Categories
derive instance newtypeCategories :: Newtype Categories _
derive newtype instance semigroupCategories :: Semigroup Categories
derive newtype instance monoidCategories :: Monoid Categories
instance readCategories :: Read Categories where
  read = map Categories <<< traverse read <<< String.split (String.Pattern ",")
instance showCategoreis :: Show Categories where
  show = Array.intercalate "," <<< map show <<< unwrap
instance encodeJsonCategories :: EncodeJson Categories where
  encodeJson = encodeJson <<< show
instance decodeJsonCategories :: DecodeJson Categories where
  decodeJson = note "" <<< read <=< decodeJson
