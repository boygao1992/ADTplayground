module Tonatona.Shopify.Options where

import RIO
import Options.Applicative (strOption, long, metavar, help)

import Tonatona.Options.Parser (HasParser, parser)

class HasShopifyOptions options where
  shopifyOptionsL :: Lens' options ShopifyOptions

-- PUBLIC https://{shopname}.myshopify.com/admin/api/{version}/{resource}.json
  -- shopname (runtime)
-- PRIVATE https://{apikey}:{password}@{shopname}.myshopify.com/admin/api/{version}/{resource}.json
  -- password (app init)
  -- shopname (app init)

data ShopifyOptions = ShopifyOptions
  { apiKey :: !ApiKey
  , apiSecret :: !ApiSecret
  , apiVersion :: !ApiVersion
  , baseUrl :: !BaseUrl
  } deriving (Eq, Show)
instance HasParser ShopifyOptions where
  parser = ShopifyOptions <$> parser <*> parser <*> parser <*> parser
instance HasShopifyOptions ShopifyOptions where
  shopifyOptionsL = id

newtype ApiKey = ApiKey { unApiKey :: Text }
  deriving newtype (Eq, Show, IsString)
_apiKey :: Lens' ShopifyOptions Text
_apiKey = lens (unApiKey . apiKey) \x y -> x { apiKey = ApiKey y }
instance HasParser ApiKey where
  parser =
    strOption
    $ long "spk"
    <> metavar "Api Key"
    <> help "set Shopify Api Key"

newtype ApiSecret = ApiSecret { unApiSecret :: Text }
  deriving newtype (Eq, Show, IsString)
_apiSecret :: Lens' ShopifyOptions Text
_apiSecret = lens (unApiSecret . apiSecret) \x y -> x { apiSecret = ApiSecret y }
instance HasParser ApiSecret where
  parser =
    strOption
    $ long "sps"
    <> metavar "Api Secret"
    <> help "set Shopify Api Secret"

newtype ApiVersion = ApiVersion { unApiVersion :: Text }
  deriving newtype (Eq, Show, IsString)
_apiVersion :: Lens' ShopifyOptions Text
_apiVersion = lens (unApiVersion . apiVersion) \x y -> x { apiVersion = ApiVersion y }
instance HasParser ApiVersion where
  parser =
    strOption
    $ long "spv"
    <> metavar "Api Version"
    <> help "set Shopify Api Version"

newtype BaseUrl = BaseUrl { unBaseUrl :: Text }
  deriving newtype (Eq, Show, IsString)
_baseUrl :: Lens' ShopifyOptions Text
_baseUrl = lens (unBaseUrl . baseUrl) \x y -> x { baseUrl = BaseUrl y }
instance HasParser BaseUrl where
  parser =
    strOption
    $ long "spu"
    <> metavar "Base Url"
    <> help "set Shopify App Base Url"
