{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Products where

import RIO
import Servant

import Servant.Client.Core (AuthenticatedRequest)
import Shopify.Data.Products.ProductId (ProductId)
import Shopify.Data.Products.Product (Products, SingleProduct)
import Shopify.Api.Products.Req.CountProducts as CountProducts
import Shopify.Api.Products.Req.GetProducts as GetProducts
import Shopify.Api.Products.Req.Data.PublishedStatus (PublishedStatus)
import Shopify.Api.Admin.OAuth.Data.AccessToken (accessTokenL)

import Shopify.Servant.DotJSON (DotJSON)
import Shopify.Servant.Client.Util (getApiClients, mkShopifyAuthenticateReq)
import Shopify.TestApp.Types (ApiHttpClientResources)

type Api
  = GetProducts
  :<|> GetSingleProduct
  :<|> CountProducts
  :<|> CreateProduct
  :<|> UpdateProduct
  :<|> DeleteProduct

_getProducts
  :<|> _getProductById
  :<|> _countProducts
  :<|> _createProduct
  :<|> _updateProduct
  :<|> _deleteProduct
  = getApiClients (Proxy @Api)

----------------
-- Type Synonyms

-- /products
type ProductApi a
  = AuthProtect "shopify-access-token"
  :> "products"
  :> a

-- /products/#{product_id}
type SingleProductApi a
  = ProductApi
  ( Capture "product_id" ProductId
  :> a
  )

--------------
-- GetProducts

-- GET /admin/api/2019-04/products.json
-- Retrieves a list of products
type GetProducts
  = ProductApi
  ( DotJSON
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
  )

_getProducts :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> Maybe Word8 -> Maybe ProductId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe PublishedStatus -> Maybe Text -> Maybe Text -> RIO ApiHttpClientResources Products

getProducts :: GetProducts.Req -> RIO ApiHttpClientResources Products
getProducts (GetProducts.Req _ids _limit _since_id _title _vendor _handle _product_type _collection_id _created_at_min _created_at_max _updated_at_min _updated_at_max _published_at_min _published_at_max _published_status _fields _presentment_currencies) = do
  token <- view accessTokenL
  _getProducts (mkShopifyAuthenticateReq token)
    _ids _limit _since_id _title _vendor _handle _product_type _collection_id _created_at_min _created_at_max _updated_at_min _updated_at_max _published_at_min _published_at_max _published_status _fields _presentment_currencies

-------------------
-- GetSingleProduct

-- GET /admin/api/2019-04/products/#{product_id}.json
-- Retrieves a single product
type GetSingleProduct
  = SingleProductApi
  ( DotJSON
  :> QueryParam "fields" Text -- TODO parse
  -- A comma-separated list of fields to include in the response.
  :> Get '[JSON] SingleProduct
  )

_getProductById :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ProductId -> Maybe Text -> RIO ApiHttpClientResources SingleProduct

getProductById :: ProductId -> Maybe Text -> RIO ApiHttpClientResources SingleProduct
getProductById _pid _fields = do
  token <- view accessTokenL
  _getProductById (mkShopifyAuthenticateReq token) _pid _fields

----------------
-- CountProducts

-- GET /admin/api/2019-04/products/count.json
-- Retrieves a count of products
type CountProducts
  = ProductApi
  ( "count" :> DotJSON
  :> QueryParam "vendor" Text
  -- Filter results by product vendor.
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
  :> Get '[JSON] CountProducts.Res
  )

_countProducts :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe PublishedStatus -> RIO ApiHttpClientResources Res

countProducts :: CountProducts.Req -> RIO ApiHttpClientResources CountProducts.Res
countProducts (CountProducts.Req __vendor __product_type __collection_id __created_at_min __created_at_max __updated_at_min __updated_at_max __published_at_min __published_at_max __published_status)= do
  token <- view accessTokenL
  _countProducts (mkShopifyAuthenticateReq token) __vendor __product_type __collection_id __created_at_min __created_at_max __updated_at_min __updated_at_max __published_at_min __published_at_max __published_status

----------------
-- CreateProduct

-- POST /admin/api/2019-04/products.json
-- Creates a new product
type CreateProduct
  = AuthProtect "shopify-access-token"
  :> ReqBody '[JSON] SingleProduct
  :> Post '[JSON] SingleProduct

_createProduct :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> SingleProduct -> RIO ApiHttpClientResources SingleProduct

createProduct :: SingleProduct -> RIO ApiHttpClientResources SingleProduct
createProduct singleproduct = do
  token <- view accessTokenL
  _createProduct (mkShopifyAuthenticateReq token) singleproduct

----------------
-- UpdateProduct

-- PUT /admin/api/2019-04/products/#{product_id}.json
-- Updates a product
type UpdateProduct
  = SingleProductApi
  ( DotJSON
  :> ReqBody '[JSON] SingleProduct
  :> Put '[JSON] SingleProduct
  )

_updateProduct :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ProductId -> SingleProduct -> RIO ApiHttpClientResources SingleProduct

updateProduct :: ProductId -> SingleProduct -> RIO ApiHttpClientResources SingleProduct
updateProduct pid singleproduct = do
  token <- view accessTokenL
  _updateProduct (mkShopifyAuthenticateReq token) pid singleproduct

----------------
-- DeleteProduct

-- DELETE /admin/api/2019-04/products/#{product_id}.json
-- Deletes a product
type DeleteProduct
  = SingleProductApi
  ( DotJSON
  :> Delete '[JSON] NoContent
  )

_deleteProduct :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> ProductId -> RIO ApiHttpClientResources NoContent

deleteProduct :: ProductId -> RIO ApiHttpClientResources NoContent
deleteProduct pid = do
  token <- view accessTokenL
  _deleteProduct (mkShopifyAuthenticateReq token) pid
