{-# LANGUAGE UndecidableInstances #-}
module Magento.Data.FrontendInput where

import RIO
import RIO.Partial (read)
import RIO.Text (pack, unpack)
import Text.Util (capitalize, uncapitalize)

import Database.Beam.MySQL (MySQL)
import Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax, fromBackendRow, sqlValueSyntax)

data FrontendInput
  = Select
  | Text
  | Date
  | Hidden
  | Boolean
  | Multiline
  | Textarea
  | Image
  | Multiselect
  | Price
  | Weight
  | Media_image
  | Gallery
  deriving (Eq, Ord, Read, Show, Enum)

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be FrontendInput where
  sqlValueSyntax = sqlValueSyntax . uncapitalize . pack . show

instance FromBackendRow MySQL FrontendInput where
  fromBackendRow = read . unpack . capitalize <$> fromBackendRow
