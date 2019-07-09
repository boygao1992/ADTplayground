module Shopify.Api.Admin.OAuth.Data.AccessMode where

import RIO
import Servant

data AccessMode
  = PerUser
  deriving (Eq, Ord, Show, Enum)
instance FromHttpApiData AccessMode where
  parseUrlPiece "per-user" = Right PerUser
  parseUrlPiece _ = Left "Unrecognized OAuth access mode"
instance ToHttpApiData AccessMode where
  toQueryParam PerUser = "per-user"
