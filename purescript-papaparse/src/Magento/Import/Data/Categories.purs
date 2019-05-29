module Magento.Import.Data.Categories where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Array as Array
import Data.String.Read (class Read, read)
import Data.String as String

newtype Category = Category (Array String)
derive newtype instance eqCategory :: Eq Category
derive instance newtypeCategory :: Newtype Category _
derive newtype instance semigroupCategory :: Semigroup Category
derive newtype instance monoidCategory :: Monoid Category
instance readCategory :: Read Category where
  read = Just <<< Category <<< map String.trim <<< String.split (String.Pattern "/")
instance showCategory :: Show Category where
  show = Array.intercalate "/" <<< unwrap

newtype Categories = Categories (Array Category)
derive newtype instance eqCategories :: Eq Categories
derive instance newtypeCategories :: Newtype Categories _
derive newtype instance semigroupCategories :: Semigroup Categories
derive newtype instance monoidCategories :: Monoid Categories
instance readCategories :: Read Categories where
  read = map Categories <<< traverse read <<< String.split (String.Pattern ",")
instance showCategoreis :: Show Categories where
  show = Array.intercalate "," <<< map show <<< unwrap
