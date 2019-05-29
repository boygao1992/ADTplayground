module Magento.Import.Data.ProductVisibility where

import Prelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Maybe (Maybe(..))
import Data.String.Read (class Read)

data ProductVisibility
  = Hidden
  | NotVisible
  | InCatalog
  | InSearch
  | BothCatalogAndSearch

derive instance genericProductVisibility :: Generic ProductVisibility _
derive instance eqProductVisibility :: Eq ProductVisibility
derive instance ordProductVisibility :: Ord ProductVisibility
instance enumProductVisibility :: Enum ProductVisibility where
  succ = genericSucc
  pred = genericPred
instance boundedProductVisibility :: Bounded ProductVisibility where
  top = genericTop
  bottom = genericBottom
instance boundedEnumProductVisibility :: BoundedEnum ProductVisibility where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
instance readProductVisibility :: Read ProductVisibility where
  read "Hidden" = Just Hidden
  read "Not Visible Individually" = Just NotVisible
  read "Catalog" = Just InCatalog
  read "Search" = Just InSearch
  read "Catalog, Search" = Just BothCatalogAndSearch
  read _ = Nothing
instance showProductVisibility :: Show ProductVisibility where
  show Hidden = "Hidden"
  show NotVisible = "Not Visible Individually"
  show InCatalog = "Catalog"
  show InSearch = "Search"
  show BothCatalogAndSearch = "Catalog, Search"
