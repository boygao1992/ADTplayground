{-# LANGUAGE UndecidableInstances #-}
module Shopify.TestApp.Data.Role where

import RIO
import RIO.Partial (read)
import RIO.Text (pack, unpack)
import Database.Beam.MySQL (MySQL)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Migrate.Generics
import Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax, fromBackendRow, sqlValueSyntax)
import Database.Beam.Backend.SQL.SQL92 (varCharType)

data Role
  = Admin
  | Senior
  | Junior
  deriving (Eq, Ord, Show, Enum, Read)

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Role where
  sqlValueSyntax = sqlValueSyntax . pack . show

instance FromBackendRow MySQL Role where
  fromBackendRow = read . unpack <$> fromBackendRow
instance FromBackendRow Postgres Role where
  fromBackendRow = read . unpack <$> fromBackendRow

instance HasDefaultSqlDataType Postgres Role where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing
