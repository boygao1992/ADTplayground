{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Customers.Addresses where

import RIO
import Servant
import Servant.Client.Core (AuthenticatedRequest)

import Shopify.Api.Admin.OAuth.Data.AccessToken (accessTokenL)
import Shopify.Api.Customers.Addresses.Req.GetAddresses as GetAddresses
import Shopify.Api.Customers.Addresses.Req.Data.AddressOperation (AddressOperation)
import Shopify.Api.Customers.Addresses.Req.DeleteAddress as DeleteAddress (Res)
import Shopify.Data.Customers.CustomerId (CustomerId)
import Shopify.Data.Customers.Addresses.AddressId (AddressId)
import Shopify.Data.Customers.Addresses.Address (Addresses, SingleAddress)
import Shopify.Servant.Client.Util (getApiClients, mkShopifyAuthenticateReq)
import Shopify.Servant.DotJSON (DotJSON)
import Shopify.TestApp.Types (ApiHttpClientResources)

type Api
  = GetAddresses
  :<|> GetAddressById
  :<|> CreateAddress
  :<|> UpdateAddress
  :<|> DeleteAddress
  :<|> BulkAddressOp
  :<|> SetDefaultAddress

_getAddresses
  :<|> _getAddressById
  :<|> _createAddress
  :<|> _updateAddress
  :<|> _deleteAddress
  :<|> _bulkAddressOp
  :<|> _setDefaultAddress
  = getApiClients (Proxy @Api)

----------------
-- Type Synonyms

-- /customers/#{customer_id}/addresses
type AddressesApi a
  = AuthProtect "shopify-access-token"
  :> "customers"
  :> Capture "customer_id" CustomerId
  :> "addresses"
  :> a

-- /customers/#{customer_id}/addresses/#{address_id}
type SingleAddressApi a
  = AddressesApi
  ( Capture "address_id" AddressId
  :> a
  )

-------------
-- GetAddress

type GetAddresses
  = AddressesApi
  ( DotJSON
  :> QueryParam "limit" Word64
  :> QueryParam "page" Word64
  :> Get '[JSON] Addresses
  )
-- GET /admin/api/2019-04/customers/#{customer_id}/addresses.json
-- Retrieves a list of addresses for a customer

_getAddresses
  :: AuthenticatedRequest (AuthProtect "shopify-access-token")
  -> CustomerId
  -> Maybe Word64
  -> Maybe Word64
  -> RIO ApiHttpClientResources Addresses

getAddresses :: CustomerId -> GetAddresses.Req -> RIO ApiHttpClientResources Addresses
getAddresses customerId (GetAddresses.Req _limit _page) = do
  token <- view accessTokenL
  _getAddresses (mkShopifyAuthenticateReq token) customerId _limit _page

-----------------
-- GetAddressById

type GetAddressById
  = SingleAddressApi
  ( DotJSON
  :> Get '[JSON] SingleAddress
  )
-- GET /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
-- Retrieves details for a single customer address

_getAddressById
  :: AuthenticatedRequest (AuthProtect "shopify-access-token")
  -> CustomerId
  -> AddressId
  -> RIO ApiHttpClientResources SingleAddress

getAddressById
  :: CustomerId
  -> AddressId
  -> RIO ApiHttpClientResources SingleAddress
getAddressById customerId addressId = do
  token <- view accessTokenL
  _getAddressById (mkShopifyAuthenticateReq token) customerId addressId

----------------
-- CreateAddress

type CreateAddress
  = AddressesApi
  ( DotJSON
  :> ReqBody '[JSON] SingleAddress
  :> Post '[JSON] SingleAddress
  )
-- POST /admin/api/2019-04/customers/#{customer_id}/addresses.json
-- Creates a new address for a customer

_createAddress
  :: AuthenticatedRequest (AuthProtect "shopify-access-token")
  -> CustomerId
  -> SingleAddress
  -> RIO ApiHttpClientResources SingleAddress

createAddress
  :: CustomerId
  -> SingleAddress
  -> RIO ApiHttpClientResources SingleAddress
createAddress customerId address = do
  token <- view accessTokenL
  _createAddress (mkShopifyAuthenticateReq token) customerId address

----------------
-- UpdateAddress

type UpdateAddress
  = SingleAddressApi
  ( DotJSON
  :> ReqBody '[JSON] SingleAddress
  :> Put '[JSON] SingleAddress
  )
-- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
-- Updates an existing customer address

_updateAddress
  :: AuthenticatedRequest (AuthProtect "shopify-access-token")
  -> CustomerId
  -> AddressId
  -> SingleAddress
  -> RIO ApiHttpClientResources SingleAddress

updateAddress
  :: CustomerId
  -> AddressId
  -> SingleAddress
  -> RIO ApiHttpClientResources SingleAddress
updateAddress customerId addressId address = do
  token <- view accessTokenL
  _updateAddress (mkShopifyAuthenticateReq token) customerId addressId address

----------------
-- DeleteAddress

type DeleteAddress
  = SingleAddressApi
  ( DotJSON
  :> Delete '[JSON] DeleteAddress.Res
  )
-- DELETE /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
-- Removes an address from a customer’s address list

_deleteAddress
  :: AuthenticatedRequest (AuthProtect "shopify-access-token")
  -> CustomerId
  -> AddressId
  -> RIO ApiHttpClientResources DeleteAddress.Res

deleteAddress
  :: CustomerId
  -> AddressId
  -> RIO ApiHttpClientResources DeleteAddress.Res
deleteAddress customerId addressId = do
  token <- view accessTokenL
  _deleteAddress (mkShopifyAuthenticateReq token) customerId addressId

----------------
-- BulkAddressOp

type BulkAddressOp
  = AddressesApi
  ( "set" :> DotJSON
  :> QueryParams "address_ids" AddressId
  :> QueryParam "operation" AddressOperation
  :> Put '[JSON] NoContent
  )
-- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/set.json?address_ids[]=1053317329&operation=destroy
-- Performs bulk operations for multiple customer addresses

_bulkAddressOp
  :: AuthenticatedRequest (AuthProtect "shopify-access-token")
  -> CustomerId
  -> [AddressId]
  -> Maybe AddressOperation
  -> RIO ApiHttpClientResources NoContent

bulkAddressOp
  :: CustomerId
  -> [AddressId]
  -> Maybe AddressOperation
  -> RIO ApiHttpClientResources NoContent
bulkAddressOp customerId addressIds mOp = do
  token <- view accessTokenL
  _bulkAddressOp (mkShopifyAuthenticateReq token) customerId addressIds mOp

--------------------
-- SetDefaultAddress

type SetDefaultAddress
  = SingleAddressApi
  ( "default" :> DotJSON
  :> Put '[JSON] SingleAddress
  )
-- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}/default.json
-- Sets the default address for a customer

_setDefaultAddress
  :: AuthenticatedRequest (AuthProtect "shopify-access-token")
  -> CustomerId
  -> AddressId
  -> RIO ApiHttpClientResources SingleAddress

setDefaultAddress
  :: CustomerId
  -> AddressId
  -> RIO ApiHttpClientResources SingleAddress
setDefaultAddress customerId addressIds = do
  token <- view accessTokenL
  _setDefaultAddress (mkShopifyAuthenticateReq token) customerId addressIds
