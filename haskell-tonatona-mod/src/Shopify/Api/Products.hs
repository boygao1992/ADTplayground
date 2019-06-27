{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Products where

import RIO
import Servant

import Servant.Client.Core (AuthenticatedRequest)
import Shopify.Data.Products.ProductId (ProductId)
import Shopify.Data.Products.Product (Products, SingleProduct)
import Shopify.Api.Products.Req.GetProducts as GetProducts
import Shopify.Api.Products.Req.Data.PublishedStatus (PublishedStatus)
import Shopify.Api.Admin.OAuth.Data.AccessToken (accessTokenL)

import Shopify.Servant.DotJSON (DotJSON)
import Shopify.Servant.Client.Util (getApiClients, mkShopifyAuthenticateReq)
import Shopify.TestApp.Types (ApiHttpClientResources)

type Api = "products" :> ProductApi

type ProductApi
  = GetProducts
  :<|> GetSingleProduct

type GetProducts
  = AuthProtect "shopify-access-token"
  :> DotJSON
  :> QueryParam "ids" Text -- TODO implement ToHTTPApiData for CustomerIds
  -- Return only products specified by a comma-separated list of product IDs.
  :> QueryParam "limit" Word8
  -- Return up to this many results per page.
  -- (default: 50, maximum: 250)
  :> QueryParam "since_id" ProductId
  -- Restrict results to those after the specified ID.
  :> QueryParam "title" Text
  -- Filter results by product title.
  :> QueryParam "vendor" Text
  -- Filter results by product vendor.
  :> QueryParam "handle" Text
  -- Filter results by product handle.
  -- e.g. "ipod-nano"
  :> QueryParam "product_type" Text
  -- Filter results by product type.
  :> QueryParam "collection_id" Text -- TODO smart_collection_id custom_collection_id
  -- Filter results by product collection ID.
  :> QueryParam "created_at_min" Text
  -- Show products created after date. (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "created_at_max" Text
  -- Show products created before date. (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "updated_at_min" Text
  -- Show products last updated after date. (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "updated_at_max" Text
  -- Show products last updated before date. (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "published_at_min" Text
  -- Show products published after date. (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "published_at_max" Text
  -- Show products published before date. (format: 2014-04-25T16:15:47-04:00)
  :> QueryParam "published_status" PublishedStatus
  -- Return products by their published status
  -- (default: any)
  --   - published: Show only published products.
  --   - unpublished: Show only unpublished products.
  --   - any: Show all products.
  :> QueryParam "fields" Text -- TODO parse
  -- Show only certain fields, specified by a comma-separated list of field names.
  :> QueryParam "presentment_currencies" Text -- TODO parse
  -- Return presentment prices in only certain currencies, specified by a comma-separated list of ISO 4217 currency codes.
  :> Get '[JSON] Products
  -- GET /admin/api/2019-04/products.json
  -- Retrieves a list of products

type GetSingleProduct
  = AuthProtect "shopify-access-token"
  :> Capture "product_id" ProductId
  :> DotJSON
  :> QueryParam "fields" Text -- TODO parse
  -- A comma-separated list of fields to include in the response.
  :> Get '[JSON] SingleProduct
  -- GET /admin/api/2019-04/products/#{product_id}.json
  -- Retrieves a single product

-- TODO
-- GET /admin/api/2019-04/products/count.json
-- Retrieves a count of products


-- TODO
-- POST /admin/api/2019-04/products.json
-- Creates a new product
-- PUT /admin/api/2019-04/products/#{product_id}.json
-- Updates a product
-- DELETE /admin/api/2019-04/products/#{product_id}.json
-- Deletes a product


--------------
-- getProducts

getProducts :: GetProducts.Req -> RIO ApiHttpClientResources Products
getProducts (GetProducts.Req _ids _limit _since_id _title _vendor _handle _product_type _collection_id _created_at_min _created_at_max _updated_at_min _updated_at_max _published_at_min _published_at_max _published_status _fields _presentment_currencies) = do
  token <- view accessTokenL
  _getProducts (mkShopifyAuthenticateReq token)
    _ids _limit _since_id _title _vendor _handle _product_type _collection_id _created_at_min _created_at_max _updated_at_min _updated_at_max _published_at_min _published_at_max _published_status _fields _presentment_currencies

_getProducts :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> Maybe Word8 -> Maybe ProductId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe PublishedStatus -> Maybe Text -> Maybe Text -> RIO ApiHttpClientResources Products

-----------------
-- getProductById

getProductById :: ProductId -> Maybe Text -> RIO ApiHttpClientResources SingleProduct
getProductById pid fields = do
  token <- view accessTokenL
  _getProductById (mkShopifyAuthenticateReq token) pid fields

_getProductById :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ProductId -> Maybe Text -> RIO ApiHttpClientResources SingleProduct

------------------
-- Client Requests

_getProducts
  :<|> _getProductById
  = getApiClients (Proxy @Api)
