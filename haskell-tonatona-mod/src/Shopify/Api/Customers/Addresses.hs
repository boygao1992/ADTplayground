{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Customers.Addresses where

import RIO
import Servant

import Shopify.Api.Customers.Addresses.Req.GetAddresses as GetAddresses
import Shopify.Api.Customers.Addresses.Req.Data.AddressOperation (AddressOperation)
import Shopify.Data.Response (Errors)
import Shopify.Data.Customers.CustomerId (CustomerId)
import Shopify.Data.Customers.Addresses.AddressId (AddressId)
import Shopify.Data.Customers.Addresses.Address (Addresses, SingleAddress)
import Shopify.Servant.Client.Util (getApiClients)
import Shopify.TestApp.Types (ApiHttpClientResources)

type Api = "customers" :> AddressApi

type AddressApi
  = GetAddresses
  :<|> GetAddressById
  :<|> CreateAddress
  :<|> UpdateAddress
  :<|> DeleteAddress
  :<|> BulkAddressOp
  :<|> SetDefaultAddress

( _getAddresses
  :<|> getAddressById
  :<|> createAddress
  :<|> updateAddress
  :<|> deleteAddress
  :<|> bulkAddressOp
  :<|> setDefaultAddress ) = getApiClients (Proxy @Api)

-------------
-- GetAddress

type GetAddresses
  = Capture "customer_id" CustomerId
    :> "addresses"
    :> QueryParam "limit" Word64
    :> QueryParam "page" Word64
    :> Get '[JSON] Addresses
    -- GET /admin/api/2019-04/customers/#{customer_id}/addresses.json
    -- Retrieves a list of addresses for a customer

_getAddresses :: CustomerId -> Maybe Word64 -> Maybe Word64 -> RIO ApiHttpClientResources Addresses

getAddresses :: CustomerId -> GetAddresses.Req -> RIO ApiHttpClientResources Addresses
getAddresses customerId (GetAddresses.Req _limit _page) = _getAddresses
  customerId _limit _page

-----------------
-- GetAddressById

type GetAddressById
  = Capture "customer_id" CustomerId
  :> "addresses"
  :> Capture "address_id" AddressId
  :> Get '[JSON] SingleAddress
  -- GET /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
  -- Retrieves details for a single customer address

getAddressById :: CustomerId -> AddressId -> RIO ApiHttpClientResources SingleAddress

----------------
-- CreateAddress

type CreateAddress
  = Capture "customer_id" CustomerId
    :> "addresses"
    :> ReqBody '[JSON] SingleAddress
    :> Post '[JSON] SingleAddress
    -- POST /admin/api/2019-04/customers/#{customer_id}/addresses.json
    -- Creates a new address for a customer

createAddress :: CustomerId -> SingleAddress -> RIO ApiHttpClientResources SingleAddress

----------------
-- UpdateAddress

type UpdateAddress
  = Capture "customer_id" CustomerId
  :> "addresses"
  :> ReqBody '[JSON] SingleAddress
  :> Put '[JSON] SingleAddress
  -- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
  -- Updates an existing customer address

updateAddress :: CustomerId -> SingleAddress -> RIO ApiHttpClientResources SingleAddress

----------------
-- DeleteAddress

type DeleteAddress
  = Capture "customer_id" CustomerId
    :> "addresses"
    :> Capture "address_id" AddressId
    :> Delete '[JSON] Errors
    -- DELETE /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
    -- Removes an address from a customer’s address list

deleteAddress :: CustomerId -> AddressId -> RIO ApiHttpClientResources Errors

----------------
-- BulkAddressOp

type BulkAddressOp
  = Capture "customer_id" CustomerId
  :> "addresses"
  :> "set"
  :> QueryParams "address_ids" AddressId
  :> QueryParam "operation" AddressOperation
  :> Put '[JSON] Errors
  -- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/set.json?address_ids[]=1053317329&operation=destroy
  -- Performs bulk operations for multiple customer addresses

bulkAddressOp :: CustomerId -> [AddressId] -> Maybe AddressOperation -> RIO ApiHttpClientResources Errors

--------------------
-- SetDefaultAddress

type SetDefaultAddress
  = Capture "customer_id" CustomerId
  :> "addresses"
  :> Capture "address_id" AddressId
  :> "default"
  :> Put '[JSON] SingleAddress
  -- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}/default.json
  -- Sets the default address for a customer

setDefaultAddress :: CustomerId -> AddressId -> RIO ApiHttpClientResources SingleAddress

