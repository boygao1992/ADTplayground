module Shopify.Api.Admin.OAuth.Data.AccessToken where

import RIO
import Data.Aeson (FromJSON, ToJSON)
import Servant (FromHttpApiData, ToHttpApiData)

class HasAccessToken env where
  accessTokenL :: Lens' env AccessToken

newtype AccessToken = AccessToken { unAccessToken :: Text }
  deriving newtype (Eq, Ord, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
  deriving (Show)
instance HasAccessToken AccessToken where
  accessTokenL = id
