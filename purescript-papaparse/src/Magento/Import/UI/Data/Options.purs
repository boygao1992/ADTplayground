module Magento.Import.Data.Options where

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))

type OptionId = String

type Options = Map OptionId Boolean

defaultOptions :: Options
defaultOptions = Map.fromFoldable
  [ Tuple "sku" true
  , Tuple "category" true
  , Tuple "product_type" true
  ]
