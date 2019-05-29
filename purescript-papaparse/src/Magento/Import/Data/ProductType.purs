module Magento.Import.Data.ProductType where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Read (class Read)

data ProductType
  = Simple
  | Grouped
  | Configurable
  | Virtual
  | Bundle
  | Downloadable

derive instance genericProductType :: Generic ProductType _
derive instance eqProductType :: Eq ProductType
derive instance ordProductType :: Ord ProductType
instance readProductType :: Read ProductType where
  read "simple"       = Just Simple
  read "grouped"      = Just Grouped
  read "configurable" = Just Configurable
  read "virtual"      = Just Virtual
  read "bundle"       = Just Bundle
  read "downloadable" = Just Downloadable
  read _ = Nothing
instance showProductType :: Show ProductType where
  show = String.toLower <<< genericShow
