{-# LANGUAGE UndecidableInstances #-}
module Magento.Data.CategoryPath where

import RIO
import RIO.Partial (read)
import RIO.List (intercalate)
import Text.Read (readsPrec)
import Data.ByteString.Char8 (split, unpack)
import Database.Beam.MySQL (MySQL)
import Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax, fromBackendRow, sqlValueSyntax)

newtype CategoryPath = CategoryPath { unCategoryPath :: [Word32] }
  deriving newtype (Eq)
instance Read CategoryPath where
  readsPrec _ input
    = [(CategoryPath . fmap (read . unpack) . split '/' . fromString $ input , "")]
instance Show CategoryPath where
  show = intercalate "/" . fmap show . unCategoryPath

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CategoryPath where
  sqlValueSyntax = sqlValueSyntax . show

instance FromBackendRow MySQL CategoryPath where
  fromBackendRow = read . unpack <$> fromBackendRow
