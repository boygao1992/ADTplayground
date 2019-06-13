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

import Tonatona.Servant.Run (HasServantClientEnv, ServantClientEnv, servantClientEnvL)

import Shopify.Api.Admin.OAuth.Data.AccessToken (HasAccessToken, AccessToken, accessTokenL)


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
instance HasShopifyOptions Resources where
  shopifyOptionsL = shopifyResourcesL . shopifyOptionsL

--------------------
-- runHttpClientBase

data BaseHttpClientResources = BaseHttpClientResources
  { baseHttpClientResourcesAppResources :: !Resources
  , baseHttpClientResourcesClientEnv :: !ServantClientEnv
  }
_baseHttpClientAppResources :: Lens' BaseHttpClientResources Resources
_baseHttpClientAppResources = lens baseHttpClientResourcesAppResources \x y -> x { baseHttpClientResourcesAppResources = y }
instance HasServantClientEnv BaseHttpClientResources where
  servantClientEnvL = lens baseHttpClientResourcesClientEnv \x y -> x { baseHttpClientResourcesClientEnv = y }
instance HasLogFunc BaseHttpClientResources where
  logFuncL = _baseHttpClientAppResources . logFuncL
instance HasBeamMySQLResources BaseHttpClientResources where
  beamMySQLResourcesL = _baseHttpClientAppResources . beamMySQLResourcesL
instance HasServantResources BaseHttpClientResources where
  servantResourcesL = _baseHttpClientAppResources . servantResourcesL
instance HasShopifyResources BaseHttpClientResources where
  shopifyResourcesL = _baseHttpClientAppResources . shopifyResourcesL
instance HasShopifyOptions BaseHttpClientResources where
  shopifyOptionsL = shopifyResourcesL . shopifyOptionsL

-------------------
-- runHttpClientApi

data ApiHttpClientResources = ApiHttpClientResources
  { apiHttpClientResourcesAppResources :: !Resources
  , apiHttpClientResourcesClientEnv :: !ServantClientEnv
  , apiHttpClientResourcesAccessToken :: !AccessToken
  }
_apiHttpClientAppResources :: Lens' ApiHttpClientResources Resources
_apiHttpClientAppResources = lens apiHttpClientResourcesAppResources \x y -> x { apiHttpClientResourcesAppResources = y }
instance HasServantClientEnv ApiHttpClientResources where
  servantClientEnvL = lens apiHttpClientResourcesClientEnv \x y -> x { apiHttpClientResourcesClientEnv = y }
instance HasAccessToken ApiHttpClientResources where
  accessTokenL = lens apiHttpClientResourcesAccessToken \x y -> x { apiHttpClientResourcesAccessToken = y }
instance HasLogFunc ApiHttpClientResources where
  logFuncL = _apiHttpClientAppResources . logFuncL
instance HasBeamMySQLResources ApiHttpClientResources where
  beamMySQLResourcesL = _apiHttpClientAppResources . beamMySQLResourcesL
instance HasServantResources ApiHttpClientResources where
  servantResourcesL = _apiHttpClientAppResources . servantResourcesL
instance HasShopifyResources ApiHttpClientResources where
  shopifyResourcesL = _apiHttpClientAppResources . shopifyResourcesL
instance HasShopifyOptions ApiHttpClientResources where
  shopifyOptionsL = shopifyResourcesL . shopifyOptionsL
