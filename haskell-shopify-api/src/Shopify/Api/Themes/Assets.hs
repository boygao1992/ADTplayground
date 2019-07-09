{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Themes.Assets where

import RIO
import Servant
import Servant.Client.Core (AuthenticatedRequest)

import Shopify.Api.Admin.OAuth.Data.AccessToken (accessTokenL)
import Shopify.Data.Themes.ThemeId (ThemeId)
import Shopify.Data.Themes.Assets.Asset (SingleAsset, Assets)
import Shopify.Data.Themes.Assets.AssetKey (AssetKey)
import Shopify.Servant.DotJSON (DotJSON)
import Shopify.Servant.Client.Util (getApiClients, mkShopifyAuthenticateReq)
import Shopify.TestApp.Types (ApiHttpClientResources)

type Api
  = GetAssets
  :<|> GetAssetByKey
  :<|> CreateOrUpdateAsset
  :<|> DeleteAsset

_getAssets
  :<|> _getAssetByKey
  :<|> _createOrUpdateAsset
  :<|> _deleteAsset
  = getApiClients (Proxy @Api)

----------------
-- Type Synonyms

-- /themes/#{theme_id}/assets.json
type AssetsJsonApi a
  = AuthProtect "shopify-access-token"
  :> "themes"
  :> Capture "theme_id" ThemeId
  :> "assets" :> DotJSON
  :> a

type SingleAssetJsonApi a
  = AssetsJsonApi
  ( QueryParam' [Strict, Required] "asset[key]" AssetKey
  :> a
  )

------------
-- GetAssets

type GetAssets
  = AssetsJsonApi
  ( QueryParam "fields" Text
  :> Get '[JSON] Assets
  )
-- GET /admin/api/2019-04/themes/#{theme_id}/assets.json
-- Retrieves a list of assets for a theme

_getAssets :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ThemeId -> Maybe Text -> RIO ApiHttpClientResources Assets

getAssets :: ThemeId -> Maybe Text -> RIO ApiHttpClientResources Assets
getAssets themeId mFields = do
  token <- view accessTokenL
  _getAssets (mkShopifyAuthenticateReq token) themeId mFields

----------------
-- GetAssetByKey

type GetAssetByKey
  = SingleAssetJsonApi
  ( Get '[JSON] SingleAsset
  )
-- GET /admin/api/2019-04/themes/#{theme_id}/assets.json?asset[key]=templates/index.liquid&theme_id=828155753
-- Retrieves a single asset for a theme

_getAssetByKey :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ThemeId -> AssetKey -> RIO ApiHttpClientResources SingleAsset

getAssetByKey :: ThemeId -> AssetKey -> RIO ApiHttpClientResources SingleAsset
getAssetByKey themeId assetKey = do
  token <- view accessTokenL
  _getAssetByKey (mkShopifyAuthenticateReq token) themeId assetKey

----------------------
-- CreateOrUpdateAsset

type CreateOrUpdateAsset
  = AssetsJsonApi
  ( ReqBody '[JSON] SingleAsset
  :> Put '[JSON] SingleAsset
  )
-- PUT /admin/api/2019-04/themes/#{theme_id}/assets.json
-- Creates or updates an asset for a theme

_createOrUpdateAsset :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ThemeId -> SingleAsset -> RIO ApiHttpClientResources SingleAsset

createOrUpdateAsset :: ThemeId -> SingleAsset -> RIO ApiHttpClientResources SingleAsset
createOrUpdateAsset themeId asset = do
  token <- view accessTokenL
  _createOrUpdateAsset (mkShopifyAuthenticateReq token) themeId asset

--------------
-- DeleteAsset

type DeleteAsset
  = SingleAssetJsonApi
  ( Delete '[JSON] NoContent
  )
-- DELETE /admin/api/2019-04/themes/#{theme_id}/assets.json?asset[key]=assets/bg-body.gif
-- Deletes an asset from a theme

_deleteAsset :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ThemeId -> AssetKey -> RIO ApiHttpClientResources NoContent

deleteAsset :: ThemeId -> AssetKey -> RIO ApiHttpClientResources NoContent
deleteAsset themeId assetKey = do
  token <- view accessTokenL
  _deleteAsset (mkShopifyAuthenticateReq token) themeId assetKey

