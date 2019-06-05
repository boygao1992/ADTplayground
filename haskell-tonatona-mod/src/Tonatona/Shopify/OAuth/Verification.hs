{-# LANGUAGE TypeApplications #-}
module Tonatona.Shopify.OAuth.Verification where

import RIO
import RIO.List (find)

import Control.Arrow ((&&&))
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (hmac, hmacGetDigest)
import Network.HTTP.Client
import Network.HTTP.Types.URI
import Data.ByteArray.Encoding (convertToBase, Base(Base64))

parseRequest :: Request -> Maybe (ByteString, ByteString) -- (hmac, queryString)
parseRequest
  = (\(a,b) -> (,) <$> a <*> pure b)
  . ( (join . fmap snd . find ((== "hmac"). fst)) &&&
      (renderQuery False . filter ((/= "hmac") . fst))
    )
  . parseQuery
  . queryString

digestHmac :: ByteString -> ByteString -> ByteString
digestHmac secret message
  = convertToBase Base64
  . hmacGetDigest @SHA256
  $ hmac secret message

