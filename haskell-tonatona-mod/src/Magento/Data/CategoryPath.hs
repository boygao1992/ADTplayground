module Magento.Data.CategoryPath where

import RIO
import RIO.Partial (read)
import Text.Read (readsPrec)
import Data.ByteString.Char8 (split, unpack)

newtype CategoryPath = CategoryPath { unCategoryPath :: [Word32] }
  deriving newtype (Eq, Show)
instance Read CategoryPath where
  readsPrec _ input
    = [(CategoryPath . fmap (read . unpack) . split '/' . fromString $ input , "")]
