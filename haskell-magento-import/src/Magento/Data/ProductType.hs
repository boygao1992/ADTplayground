{-# LANGUAGE UndecidableInstances #-}
module Magento.Data.ProductType where

import RIO
import RIO.Partial (read)
import RIO.Text (pack, unpack)
import Text.Util (capitalize, uncapitalize)

import Database.Beam.MySQL (MySQL)
import Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax, fromBackendRow, sqlValueSyntax)

data ProductType
  = Simple
  | Grouped
  | Configurable
  | Virtual
  | Bundle
  | Downloadable
  deriving (Eq, Ord, Read, Show, Enum)

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be ProductType where
  sqlValueSyntax = sqlValueSyntax . uncapitalize . pack . show

instance FromBackendRow MySQL ProductType where
  fromBackendRow = read . unpack . capitalize <$> fromBackendRow

