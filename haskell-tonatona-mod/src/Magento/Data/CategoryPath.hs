{-# LANGUAGE UndecidableInstances #-}
module Magento.Data.CategoryPath where

import RIO
import RIO.Partial (read)
import qualified RIO.List as List (intercalate, length)
import Text.Read (readsPrec)
import Data.ByteString.Char8 (split, unpack)
import Database.Beam.MySQL (MySQL)
import Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax, fromBackendRow, sqlValueSyntax)

length :: CategoryPath -> Int
length = List.length . unCategoryPath

newtype CategoryPath = CategoryPath { unCategoryPath :: [Word32] }
  deriving newtype (Eq)
instance Read CategoryPath where
  readsPrec _ input
    = [(CategoryPath . fmap (read . unpack) . split '/' . fromString $ input , "")]
instance Show CategoryPath where
  show = List.intercalate "/" . fmap show . unCategoryPath

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CategoryPath where
  sqlValueSyntax = sqlValueSyntax . show

instance FromBackendRow MySQL CategoryPath where
  fromBackendRow = read . unpack <$> fromBackendRow
