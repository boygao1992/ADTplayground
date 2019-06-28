{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
module Shopify.Api.Admin.OAuth.Data.AccessToken where

import RIO
import Data.Aeson (FromJSON, ToJSON)
import Servant (FromHttpApiData, ToHttpApiData)
import Database.Beam.MySQL (MySQL)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Migrate.Generics
import Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax, sqlValueSyntax)

class HasAccessToken env where
  accessTokenL :: Lens' env AccessToken

newtype AccessToken = AccessToken { unAccessToken :: Text }
  deriving newtype
    ( Eq, Ord, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, IsString
    , HasDefaultSqlDataType Postgres
    , FromBackendRow Postgres
    , FromBackendRow MySQL
    )
  deriving (Show)
instance HasAccessToken AccessToken where
  accessTokenL = id

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be AccessToken where
  sqlValueSyntax = sqlValueSyntax . unAccessToken


