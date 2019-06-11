module Shopify.TestApp.Types where

import RIO

import Tonatona.Options.Parser
import Tonatona.Logger.Options
import Tonatona.Beam.MySQL.Options
import Tonatona.Servant.Options
import Tonatona.Shopify.Options (HasShopifyOptions, ShopifyOptions, shopifyOptionsL)

import Tonatona.WithResource
import Tonatona.Logger.Resources
import Tonatona.Beam.MySQL.Resources
import Tonatona.Servant.Resources
import Tonatona.Shopify.Resources (HasShopifyResources, ShopifyResources, shopifyResourcesL)

data Options = Options
  { optionsLogger :: !LoggerOptions
  , optionsBeamMySQL :: !BeamMySQLOptions
  , optionsServant :: !ServantOptions
  , optionsShopify :: !ShopifyOptions
  }
instance HasParser Options where
  parser = Options <$> parser <*> parser <*> parser <*> parser
instance HasLoggerOptions Options where
  loggerOptionsL = lens optionsLogger \x y -> x { optionsLogger = y }
instance HasBeamMySQLOptions Options where
  beamMySQLOptionsL = lens optionsBeamMySQL \x y -> x { optionsBeamMySQL = y }
instance HasServantOptions Options where
  servantOptionsL = lens optionsServant \x y -> x { optionsServant = y }
instance HasShopifyOptions Options where
  shopifyOptionsL = lens optionsShopify \x y -> x { optionsShopify = y }

data Resources = Resources
  { resourceLogFunc :: !LoggerLogFunc
  , resourceBeamMySQL :: !BeamMySQLResources
  , resourceServant :: !ServantResources
  , resourceShopify :: !ShopifyResources
  }
instance With Options Resources where
  withResource = Resources <$> withResource <*> withResource <*> withResource <*> withResource
instance HasLogFunc Resources where
  logFuncL = lens resourceLogFunc (\x y -> x { resourceLogFunc = y }) . logFuncL
instance HasBeamMySQLResources Resources where
  beamMySQLResourcesL = lens resourceBeamMySQL (\x y -> x { resourceBeamMySQL = y })
instance HasServantResources Resources where
  servantResourcesL = lens resourceServant \x y -> x { resourceServant = y }
instance HasShopifyResources Resources where
  shopifyResourcesL = lens resourceShopify \x y -> x { resourceShopify = y }
