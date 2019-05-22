{-# LANGUAGE UndecidableInstances #-}
module Magento.Data.ProductVisibility where

import RIO
import Text.Read (readsPrec)

data ProductVisibility
  = Hidden
  | NotVisible
  | InCatalog
  | InSearch
  | BothCatalogAndSearch
  deriving (Eq, Ord, Show, Enum)

instance Read ProductVisibility where
  readsPrec _ "Not Visible Individually" = [(NotVisible, "")]
  readsPrec _ "Catalog" = [(InCatalog, "")]
  readsPrec _ "Search" = [(InSearch, "")]
  readsPrec _ "Catalog, Search" = [(BothCatalogAndSearch, "")]
  readsPrec _ _ = []
