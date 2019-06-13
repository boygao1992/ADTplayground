module Tonatona.Shopify.Resources where

import RIO
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Tonatona.Shopify.Options as Options (ShopifyOptions, HasShopifyOptions, shopifyOptionsL)
import Tonatona.WithResource (With, withResource, hoistWithResource)

class HasShopifyResources resources where
  shopifyResourcesL :: Lens' resources ShopifyResources

data ShopifyResources = ShopifyResources
  { shopifyOptions :: !ShopifyOptions
  , shopifyHttpClientManager :: !ShopifyHttpClientManager
  }
instance (Options.HasShopifyOptions options) => With options ShopifyResources where
  withResource = ShopifyResources <$> withResource <*> withResource
instance HasShopifyResources ShopifyResources where
  shopifyResourcesL = id
instance Options.HasShopifyOptions ShopifyResources where
  shopifyOptionsL = _shopifyOptions

newtype ShopifyOptions = ShopifyOptions
  { unShopifyOptions :: Options.ShopifyOptions }
_shopifyOptions :: Lens' ShopifyResources Options.ShopifyOptions
_shopifyOptions = lens (unShopifyOptions . shopifyOptions) \x y -> x { shopifyOptions = ShopifyOptions y }
instance Options.HasShopifyOptions options => With options ShopifyOptions where
  withResource = hoistWithResource \options cont -> do
    let shipifyOption = options^.Options.shopifyOptionsL
    cont $ ShopifyOptions shipifyOption

newtype ShopifyHttpClientManager = ShopifyHttpClientManager { unShopifyHttpClientManager :: Manager }
_manager :: Lens' ShopifyResources Manager
_manager = lens (unShopifyHttpClientManager . shopifyHttpClientManager) \x y -> x { shopifyHttpClientManager = ShopifyHttpClientManager y }
instance With options ShopifyHttpClientManager where
  withResource = hoistWithResource \_ cont -> do
    manager <- liftIO $ newManager tlsManagerSettings
    cont $ ShopifyHttpClientManager manager

