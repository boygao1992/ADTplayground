module Shopify.Api.OAuth.Verification where

import RIO
import RIO.List (find)

import Control.Arrow ((&&&))
import Network.HTTP.Client
import Network.HTTP.Types.URI

parseRequest :: Request -> Maybe (ByteString, ByteString) -- (hmac, queryString)
parseRequest
  = (\(a,b) -> (,) <$> a <*> pure b)
  . ( (join . fmap snd . find ((== "hmac"). fst)) &&&
      (renderQuery False . filter ((/= "hmac") . fst))
    )
  . parseQuery
  . queryString
