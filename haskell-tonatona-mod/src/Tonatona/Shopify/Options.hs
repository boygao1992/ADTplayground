module Tonatona.Shopify.Options where

import RIO
import Options.Applicative (strOption, long, metavar, help, showDefault)

import Tonatona.Options.Parser (HasParser, parser)

class HasShopifyOptions options where
  shopifyOptionsL :: Lens' options ShopifyOptions

data ShopifyOptions = ShopifyOptions
  { apiKey :: !ApiKey
  , apiSecret :: !ApiSecret
  } deriving (Eq, Show)
instance HasParser ShopifyOptions where
  parser = ShopifyOptions <$> parser <*> parser
instance HasShopifyOptions ShopifyOptions where
  shopifyOptionsL = id

newtype ApiKey = ApiKey { unApiKey :: ByteString }
  deriving newtype (Eq, Show, IsString)
_apiKey :: Lens' ShopifyOptions ByteString
_apiKey = lens (unApiKey . apiKey) \x y -> x { apiKey = ApiKey y }
instance HasParser ApiKey where
  parser =
    strOption
    $ long "spk"
    <> metavar "Api Key"
    <> showDefault
    <> help "set Shopify Api Key"

newtype ApiSecret = ApiSecret { unApiSecret :: ByteString }
  deriving newtype (Eq, Show, IsString)
_apiSecret :: Lens' ShopifyOptions ByteString
_apiSecret = lens (unApiSecret . apiSecret) \x y -> x { apiSecret = ApiSecret y }
instance HasParser ApiSecret where
  parser =
    strOption
    $ long "sps"
    <> metavar "Api Secret"
    <> showDefault
    <> help "set Shopify Api Secret"
