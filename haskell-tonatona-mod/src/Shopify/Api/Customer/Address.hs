{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Customer.Address where

import RIO
import Servant
import Servant.Client (ClientM, client)
import Data.Aeson.TH
import Shopify.Data.Response (Errors)
import Shopify.Api.Customer.Data.CustomerId (CustomerId)
import Shopify.Api.Customer.Address.Data.AddressId (AddressId)

data Address = Address
  { _id :: !(Maybe AddressId)
    -- "id": 207119551,
  , _customer_id :: !(Maybe Word32)
    -- "customer_id": 207119551,
  , _first_name :: !(Maybe Text)
    -- "first_name": null,
  , _last_name :: !(Maybe Text)
    -- "last_name": null,
  , _company :: !(Maybe Text)
    -- "company": null,
  , _address1 :: !(Maybe Text)
    -- "address1": "Chestnut Street 92",
  , _address2 :: !(Maybe Text)
    -- "address2": "",
  , _city :: !(Maybe Text)
    -- "city": "Louisville",
  , _province :: !(Maybe Text)
    -- "province": "Kentucky",
  , _country :: !(Maybe Text)
    -- "country": "United States",
  , _zip :: !(Maybe Text)
    -- "zip": "40202",
  , _phone :: !(Maybe Text)
    -- "phone": "555-625-1199",
  , _name :: !(Maybe Text)
    -- "name": "",
  , _province_code :: !(Maybe Text)
    -- "province_code": "KY",
  , _country_code :: !(Maybe Text)
    -- "country_code": "US",
  , _country_name :: !(Maybe Text)
    -- "country_name": "United States",
  , _default :: !(Maybe Bool)
    -- "default": true
  , _latitude :: !(Maybe Text)
    -- "latitude": "45.41634", -- NOTE business/shipping address only
  , _longitude :: !(Maybe Text)
    -- "longitude": "-75.6868", -- NOTE business/shipping address only
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Address)

data SingleAddress = SingleAddress
  { _customer_address :: !Address
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''SingleAddress)

data Addresses = Addresses
  { _addresses :: ![Address]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Addresses)

data AddressOperation
  = AddressDestroy
  deriving (Eq, Ord, Show)
instance ToHttpApiData AddressOperation where
  toQueryParam AddressDestroy = "destroy"

type Api = "customers" :> AddressApi

type AddressApi
  = Capture "customer_id" CustomerId
    :> "addresses"
    :> QueryParam "limit" Word32
    :> QueryParam "page" Word32
    :> Get '[JSON] Addresses
    -- GET /admin/api/2019-04/customers/#{customer_id}/addresses.json
    -- Retrieves a list of addresses for a customer
  :<|> Capture "customer_id" CustomerId
    :> "addresses"
    :> Capture "address_id" AddressId
    :> Get '[JSON] SingleAddress
    -- GET /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
    -- Retrieves details for a single customer address
  :<|> Capture "customer_id" CustomerId
    :> "addresses"
    :> ReqBody '[JSON] SingleAddress
    :> Post '[JSON] SingleAddress
    -- POST /admin/api/2019-04/customers/#{customer_id}/addresses.json
    -- Creates a new address for a customer
  :<|> Capture "customer_id" CustomerId
    :> "addresses"
    :> ReqBody '[JSON] SingleAddress
    :> Put '[JSON] SingleAddress
    -- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
    -- Updates an existing customer address
  :<|> Capture "customer_id" CustomerId
    :> "addresses"
    :> Capture "address_id" AddressId
    :> Delete '[JSON] Errors
    -- DELETE /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}.json
    -- Removes an address from a customer’s address list
  :<|> Capture "customer_id" CustomerId
    :> "addresses"
    :> "set"
    :> QueryParams "address_ids" Word32
    :> QueryParam "operation" AddressOperation
    :> Put '[JSON] Errors
    -- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/set.json?address_ids[]=1053317329&operation=destroy
    -- Performs bulk operations for multiple customer addresses
  :<|> Capture "customer_id" CustomerId
    :> "addresses"
    :> Capture "address_id" AddressId
    :> "default"
    :> Put '[JSON] SingleAddress
    -- PUT /admin/api/2019-04/customers/#{customer_id}/addresses/#{address_id}/default.json
    -- Sets the default address for a customer

_getAddresses :: CustomerId -> Maybe Word32 -> Maybe Word32 -> ClientM Addresses
_getAddressById :: CustomerId -> AddressId -> ClientM SingleAddress
_createAddress :: CustomerId -> SingleAddress -> ClientM SingleAddress
_updateAddress :: CustomerId -> SingleAddress -> ClientM SingleAddress
_deleteAddress :: CustomerId -> AddressId -> ClientM Errors
_bulkAddressOp :: CustomerId -> [Word32] -> Maybe AddressOperation -> ClientM Errors
_setDefaultAddress :: CustomerId -> AddressId -> ClientM SingleAddress
( _getAddresses
  :<|> _getAddressById
  :<|> _createAddress
  :<|> _updateAddress
  :<|> _deleteAddress
  :<|> _bulkAddressOp
  :<|> _setDefaultAddress) = client (Proxy @Api)
