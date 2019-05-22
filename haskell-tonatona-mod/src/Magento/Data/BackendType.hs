{-# LANGUAGE UndecidableInstances #-}
module Magento.Data.BackendType where

import RIO
import RIO.Partial (read)
import RIO.Text (pack, unpack)
import Text.Read (readsPrec)
import Text.Util (uncapitalize)
import Database.Beam.MySQL (MySQL)
import Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax, fromBackendRow, sqlValueSyntax)

data BackendType
  = Static -- static
  | Datetime -- datetime
  | Decimal -- decimal
  | Int -- int, integer NOTE
  | Text -- text
  | Varchar -- varchar
  deriving (Eq, Ord, Show, Enum)

instance Read BackendType where
  readsPrec _ input
    | input == "static" = [(Static, "")]
    | input == "datetime" = [(Datetime, "")]
    | input == "decimal" = [(Decimal, "")]
    | input == "int" || input == "integer" = [(Int, "")]
    | input == "text" = [(Text, "")]
    | input == "varchar" = [(Varchar, "")]
    | otherwise = []

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be BackendType where
  sqlValueSyntax = sqlValueSyntax . uncapitalize . pack . show

instance FromBackendRow MySQL BackendType where
  fromBackendRow = read . unpack <$> fromBackendRow

