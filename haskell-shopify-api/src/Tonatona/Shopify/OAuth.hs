module Tonatona.Shopify.OAuth where

import RIO

import Network.HTTP.Client as Http (queryString, Request)
import Servant
import qualified Tonatona.Shopify.OAuth.Verification as Shopify
import Tonatona.Servant.Auth.OAuth
import Tonatona.Shopify.Options

-- | Inbound Request

oauthHandler
  :: ( HasShopifyOptions env
    , HasLogFunc env
    )
  => RIOAuthHandler env Http.Request NoContent
oauthHandler = mkRIOAuthHandler handler
  where
    handler req = do
      case Shopify.parseRequest req of
        Nothing -> do
          logError $ displayBytesUtf8 "OAuth Handler: unexpected query string."
          logError $ displayBytesUtf8 $ "raw: " <> queryString req
          throwM $ err400
        Just (hmacReceived, message) -> do
          secret <- encodeUtf8 <$> view (shopifyOptionsL._apiSecret)
          let hmacExpected = Shopify.digestHmac secret message
          if hmacReceived /= hmacExpected
            then do
              logError $ displayBytesUtf8 "OAuth Handler: HMAC verification failed."
              logError $ displayBytesUtf8 $ "expected: " <> hmacExpected
              logError $ displayBytesUtf8 $ "received: " <> hmacReceived
              throwM $ err401
            else
              pure NoContent
