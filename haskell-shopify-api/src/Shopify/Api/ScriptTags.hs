{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.ScriptTags where

import RIO
import Servant
import Servant.Client.Core (AuthenticatedRequest)

import Shopify.Api.Admin.OAuth.Data.AccessToken (accessTokenL)
import Shopify.Api.ScriptTags.Req.GetScriptTags as GetScriptTags (Req(..))
import Shopify.Data.ScriptTags.ScriptTag (SingleScriptTag, ScriptTags)
import Shopify.Data.ScriptTags.ScriptTagId (ScriptTagId)
import Shopify.Api.ScriptTags.Req.CountScriptTags as CountScriptTags (Res)
import Shopify.Servant.DotJSON (DotJSON)
import Shopify.Servant.Client.Util (getApiClients, mkShopifyAuthenticateReq)
import Shopify.TestApp.Types (ApiHttpClientResources)

type Api
  = GetScriptTags
  :<|> CountScriptTags
  :<|> GetScriptTagById
  :<|> CreateScriptTag
  :<|> UpdateScriptTag
  :<|> DeleteScriptTag

_getScriptTags
  :<|> _countScriptTags
  :<|> _getScriptTagById
  :<|> _createScriptTag
  :<|> _updateScriptTag
  :<|> _deleteScriptTag
  = getApiClients (Proxy @Api)

----------------
-- Type Synonyms

-- /script_tags
type ScriptTagsApi a
  = AuthProtect "shopify-access-token"
  :> "script_tags"
  :> a
-- /script_tags/#{script_tag_id}
type SingleScriptTagApi a
  = ScriptTagsApi
  ( Capture "script_tag_id" ScriptTagId
  :> a
  )

----------------
-- GetScriptTags

type GetScriptTags
  = ScriptTagsApi
  ( DotJSON
  :> QueryParam "limit" Word8
  -- The number of results to return.
  -- (default: 50, maximum: 250)
  :> QueryParam "since_id" ScriptTagId
  -- Restrict results to after the specified ID.
  :> QueryParam "created_at_min" Text -- TODO UTC
  -- Show script tags created after this date.
  -- (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "created_at_max" Text -- TODO UTC
  -- Show script tags created before this date.
  -- (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "updated_at_min" Text -- TODO UTC
  -- Show script tags last updated after this date.
  -- (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "updated_at_max" Text -- TODO UTC
  -- Show script tags last updated before this date.
  -- (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "src" Text
  -- Show script tags with this URL.
  :> QueryParam "fields" Text -- TODO Enum
  -- A comma-separated list of fields to include in the response.
  :> Get '[JSON] ScriptTags
  )
-- GET /admin/api/2019-04/script_tags.json
-- Retrieves a list of all script tags

_getScriptTags :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Word8 -> Maybe ScriptTagId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> RIO ApiHttpClientResources ScriptTags

getScriptTags
  :: GetScriptTags.Req
  -> RIO ApiHttpClientResources ScriptTags
getScriptTags (GetScriptTags.Req _limit _since_id _created_at_min _created_at_max _updated_at_min _updated_at_max _src _fields) = do
  token <- view accessTokenL
  _getScriptTags (mkShopifyAuthenticateReq token) _limit _since_id _created_at_min _created_at_max _updated_at_min _updated_at_max _src _fields

------------------
-- CountScriptTags

type CountScriptTags
  = ScriptTagsApi
  ( "count" :> DotJSON
  :> QueryParam "src" Text
  -- Count only script tags with a given URL.
  :> Get '[JSON] CountScriptTags.Res
  )
-- GET /admin/api/2019-04/script_tags/count.json
-- Retrieves a count of all script tags

_countScriptTags :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> RIO ApiHttpClientResources Res

countScriptTags
  :: Maybe Text
  -> RIO ApiHttpClientResources Res
countScriptTags mSrc = do
  token <- view accessTokenL
  _countScriptTags (mkShopifyAuthenticateReq token) mSrc

-------------------
-- GetScriptTagById

type GetScriptTagById
  = SingleScriptTagApi
  ( DotJSON
  :> QueryParam "fields" Text -- TODO
  -- A comma-separated list of fields to include in the response.
  :> Get '[JSON] SingleScriptTag
  )

-- GET /admin/api/2019-04/script_tags/#{script_tag_id}.json
-- Retrieves a single script tag

_getScriptTagById :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ScriptTagId -> Maybe Text -> RIO ApiHttpClientResources SingleScriptTag

getScriptTagById
  :: ScriptTagId
  -> Maybe Text
  -> RIO ApiHttpClientResources SingleScriptTag
getScriptTagById scriptTagId mFields = do
  token <- view accessTokenL
  _getScriptTagById (mkShopifyAuthenticateReq token) scriptTagId mFields

------------------
-- CreateScriptTag

type CreateScriptTag
  = ScriptTagsApi
  ( DotJSON
  :> ReqBody '[JSON] SingleScriptTag
  :> Post '[JSON] SingleScriptTag
  )
-- POST /admin/api/2019-04/script_tags.json
-- Creates a new script tag

_createScriptTag :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> SingleScriptTag -> RIO ApiHttpClientResources SingleScriptTag

createScriptTag
  :: SingleScriptTag
  -> RIO ApiHttpClientResources SingleScriptTag
createScriptTag scriptTag = do
  token <- view accessTokenL
  _createScriptTag (mkShopifyAuthenticateReq token) scriptTag

------------------
-- UpdateScriptTag

type UpdateScriptTag
  = SingleScriptTagApi
  ( DotJSON
  :> ReqBody '[JSON] SingleScriptTag
  :> Put '[JSON] SingleScriptTag
  )
-- PUT /admin/api/2019-04/script_tags/#{script_tag_id}.json
-- Updates a script tag

_updateScriptTag :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ScriptTagId -> SingleScriptTag -> RIO ApiHttpClientResources SingleScriptTag

updateScriptTag
  :: ScriptTagId
  -> SingleScriptTag
  -> RIO ApiHttpClientResources SingleScriptTag
updateScriptTag scriptTagId scriptTag = do
  token <- view accessTokenL
  _updateScriptTag (mkShopifyAuthenticateReq token) scriptTagId scriptTag

------------------
-- DeleteScriptTag

type DeleteScriptTag
  = SingleScriptTagApi
  ( DotJSON
  :> Delete '[JSON] NoContent
  )
-- DELETE /admin/api/2019-04/script_tags/#{script_tag_id}.json
-- Deletes a script tag

_deleteScriptTag :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ScriptTagId -> RIO ApiHttpClientResources NoContent

deleteScriptTag :: ScriptTagId -> RIO ApiHttpClientResources NoContent
deleteScriptTag scriptTagId = do
  token <- view accessTokenL
  _deleteScriptTag (mkShopifyAuthenticateReq token) scriptTagId
