{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Themes where

import RIO
import Servant
import Servant.Client.Core (AuthenticatedRequest)

import Shopify.Api.Admin.OAuth.Data.AccessToken (accessTokenL)
import Shopify.Api.Themes.Req.CreateTheme as CreateTheme (Req(..), Payload)
import Shopify.Data.Themes.Theme (SingleTheme, Themes)
import Shopify.Data.Themes.ThemeId (ThemeId)
import Shopify.Servant.DotJSON (DotJSON)
import Shopify.Servant.Client.Util (getApiClients, mkShopifyAuthenticateReq)
import Shopify.TestApp.Types (ApiHttpClientResources)

type Api
  = GetThemes
  :<|> GetThemeById
  :<|> CreateTheme
  :<|> UpdateTheme
  :<|> DeleteTheme

_getThemes
  :<|> _getThemeById
  :<|> _createTheme
  :<|> _updateTheme
  :<|> _deleteTheme
  = getApiClients (Proxy @Api)

----------------
-- Type Synonyms

-- /themes
type ThemesApi a
  = AuthProtect "shopify-access-token"
  :> "themes"
  :> a

-- /themes/#{theme_id}
type SingleThemeApi a
  = ThemesApi
  ( Capture "theme_id" ThemeId
  :> a
  )

------------
-- GetThemes

type GetThemes
  = ThemesApi
  ( DotJSON
  :> QueryParam "fields" Text
  -- Show only certain fields, specified by a comma-separated list of field names.
  :> Get '[JSON] Themes
  )
-- GET /admin/api/2019-04/themes.json
-- Retrieves a list of themes

_getThemes :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> RIO ApiHttpClientResources Themes

getThemes :: Maybe Text -> RIO ApiHttpClientResources Themes
getThemes mFields = do
  token <- view accessTokenL
  _getThemes (mkShopifyAuthenticateReq token) mFields

---------------
-- GetThemeById

type GetThemeById
  = SingleThemeApi
  ( DotJSON
  :> QueryParam "fields" Text
  -- Show only certain fields, specified by a comma-separated list of field names.
  :> Get '[JSON] SingleTheme
  )
-- GET /admin/api/2019-04/themes/#{theme_id}.json
-- Retrieves a single theme

_getThemeById :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ThemeId -> Maybe Text -> RIO ApiHttpClientResources SingleTheme

getThemeById :: ThemeId -> Maybe Text -> RIO ApiHttpClientResources SingleTheme
getThemeById themeId mFields = do
  token <- view accessTokenL
  _getThemeById (mkShopifyAuthenticateReq token) themeId mFields

--------------
-- CreateTheme

type CreateTheme
  = ThemesApi
  ( DotJSON
  :> ReqBody '[JSON] CreateTheme.Req
  :> Post '[JSON] SingleTheme
  )
-- POST /admin/api/2019-04/themes.json
-- Creates a theme

_createTheme :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Req -> RIO ApiHttpClientResources SingleTheme

createTheme :: CreateTheme.Payload -> RIO ApiHttpClientResources SingleTheme
createTheme payload = do
  token <- view accessTokenL
  _createTheme (mkShopifyAuthenticateReq token) (CreateTheme.Req payload)

--------------
-- UpdateTheme

type UpdateTheme
  = SingleThemeApi
  ( DotJSON
  :> ReqBody '[JSON] SingleTheme
  :> Put '[JSON] SingleTheme
  )
-- PUT /admin/api/2019-04/themes/#{theme_id}.json
-- Modify an existing Theme

_updateTheme :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ThemeId -> SingleTheme -> RIO ApiHttpClientResources SingleTheme

updateTheme :: ThemeId -> SingleTheme -> RIO ApiHttpClientResources SingleTheme
updateTheme themeId theme = do
  token <- view accessTokenL
  _updateTheme (mkShopifyAuthenticateReq token) themeId theme

--------------
-- DeleteTheme

type DeleteTheme
  = SingleThemeApi
  ( DotJSON
  :> Delete '[JSON] NoContent
  )
-- DELETE /admin/api/2019-04/themes/#{theme_id}.json
-- Remove a Theme from the database

_deleteTheme :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ThemeId -> RIO ApiHttpClientResources NoContent

deleteTheme :: ThemeId -> RIO ApiHttpClientResources NoContent
deleteTheme themeId = do
  token <- view accessTokenL
  _deleteTheme (mkShopifyAuthenticateReq token) themeId
