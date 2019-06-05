module Tonatona.Servant.Auth.OAuth where

import RIO
import Network.HTTP.Client
import Servant

newtype RIOAuthHandler env req res = RIOAuthHandler { unAuthHandler :: req -> RIO env res }
  deriving (Generic, Typeable)

mkRIOAuthHandler :: (req -> RIO env res) -> RIOAuthHandler env req res
mkRIOAuthHandler = RIOAuthHandler

class HasOAuthToken options where
  oAuthTokenL :: Lens' options OAuthToken

newtype OAuthToken = OAuthToken { unOAuthToken :: ByteString }
  deriving (Eq, Show, IsString)
instance HasOAuthToken OAuthToken where
  oAuthTokenL = id

