{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Customer where

-- TODO implement ad-hoc capture group to handle .json suffix gracefully

import RIO
import Servant
import Servant.Client (ClientM, client)
import Shopify.Api.Order (Orders)
import Shopify.Api.Customer.Data.Customer
import Shopify.Api.Customer.Data.CustomerId (CustomerId)
import Servant.Client.Core as Client (AuthenticatedRequest)
import Shopify.Servant.DotJSON (DotJSON)

data GetCustomersReq = GetCustomersReq
  { _ids :: Maybe Text
  , _since_id :: Maybe CustomerId
  , _created_at_min :: Maybe Text
  , _created_at_max :: Maybe Text
  , _updated_at_min :: Maybe Text
  , _updated_at_max :: Maybe Text
  , _limit :: Maybe Word8
  , _fields :: Maybe Text
  } deriving (Eq, Show)

-- data SearchCustomersReq = SearchCustomersReq
--   { _order :: Maybe Text
--   , _query :: Maybe Text
--   , _limit :: Maybe Word8
--   , _fields :: Maybe Text
--   }

type Api = "customers" :> CustomersApi
type CustomersApi
  = AuthProtect "shopify-access-token"
    :> DotJSON
    :> QueryParam "ids" Text -- TODO implement ToHTTPApiData for CustomerIds
    -- Restrict results to customers specified by a comma-separated list of IDs.
    :> QueryParam "since_id" CustomerId
    -- Restrict results to those after the specified ID.
    :> QueryParam "created_at_min" Text -- TODO UTC
    -- Show customers created after a specified date.
    -- (format: 2014-04-25T16:15:47-04:00)
    :> QueryParam "created_at_max" Text -- TODO UTC
    -- Show customers created before a specified date.
    -- (format: 2014-04-25T16:15:47-04:00)
    :> QueryParam "updated_at_min" Text -- TODO UTC
    -- Show customers last updated after a specified date.
    -- (format: 2014-04-25T16:15:47-04:00)
    :> QueryParam "updated_at_max" Text -- TODO UTC
    -- Show customers last updated before a specified date.
    -- (format: 2014-04-25T16:15:47-04:00)
    :> QueryParam "limit" Word8
    -- The maximum number of results to show.
    -- (default: 50, maximum: 250)
    :> QueryParam "fields" Text -- TODO Newtype FieldNames [FieldName]
    -- Show only certain fields, specified by a comma-separated list of field names.
    :> Get '[JSON] Customers
    -- GET /admin/api/2019-04/customers.json
    -- Retrieves a list of customers
  :<|> AuthProtect "shopify-access-token"
    :> DotJSON
    :> QueryParam "order" Text
    -- Set the field and direction by which to order results.
    -- (default: last_order_date DESC)
    :> QueryParam "query" Text
    -- Text to search for in the shop's customer data.
    :> QueryParam "limit" Word8
    -- The maximum number of results to show.
    -- (default: 50, maximum: 250)
    :> QueryParam "fields" Text
    -- Show only certain fields, specified by a comma-separated list of field names.
    :> Get '[JSON] Customers
    -- GET /admin/api/2019-04/customers/search.json?query=Bob country:United States
    -- Searches for customers that match a supplied query

  :<|> AuthProtect "shopify-access-token"
    :> Capture "customer_id" CustomerId
    :> DotJSON
    :> Get '[JSON] SingleCustomer
    -- GET /admin/api/2019-04/customers/#{customer_id}.json
    -- Retrieves a single customer

  :<|> AuthProtect "shopify-access-token"
    :> DotJSON
    :> ReqBody '[JSON] SingleCustomer
    :> Post '[JSON] SingleCustomer
    -- POST /admin/api/2019-04/customers.json
    -- Creates a customer

  :<|> AuthProtect "shopify-access-token"
    :> Capture "customer_id" CustomerId
    :> DotJSON
    :> ReqBody '[JSON] SingleCustomer
    :> Put '[JSON] SingleCustomer
    -- PUT /admin/api/2019-04/customers/#{customer_id}.json
    -- Updates a customer

  :<|> AuthProtect "shopify-access-token"
    :> Capture "customer_id" CustomerId
    :> "account_activation_url"
    :> DotJSON
    :> Post '[JSON] AccountActivationUrl
    -- POST /admin/api/2019-04/customers/#{customer_id}/account_activation_url.json
    -- Creates an account activation URL for a customer

  :<|> AuthProtect "shopify-access-token"
    :> Capture "customer_id" CustomerId
    :> "send_invite"
    :> DotJSON
    :> ReqBody '[JSON] SingleCustomerInvite
    :> Post '[JSON] SingleCustomerInvite
    -- POST /admin/api/2019-04/customers/#{customer_id}/send_invite.json
    -- Sends an account invite to a customer

  :<|> AuthProtect "shopify-access-token"
    :> Capture "customer_id" CustomerId
    :> DotJSON
    :> Delete '[JSON] DeleteCustomerResponse
    -- DELETE /admin/api/2019-04/customers/#{customer_id}.json
    -- Deletes a customer.

  :<|> AuthProtect "shopify-access-token"
    :> Capture "customer_id" CustomerId
    :> "count"
    :> DotJSON
    :> Get '[JSON] GetCustomerCountResponse
    -- GET /admin/api/2019-04/customers/count.json
    -- Retrieves a count of customers

  :<|> AuthProtect "shopify-access-token"
    :> Capture "customer_id" CustomerId
    :> "orders"
    :> DotJSON
    :> Get '[JSON] Orders
    -- GET /admin/api/2019-04/customers/#{customer_id}/orders.json
    -- Retrieves all orders belonging to a customer

-- Creating a customer without an email or name fails and returns an error

-- Create a customer with a metafield
-- "metafields": [
--   {
--     "key": "new",
--     "value": "newvalue",
--     "value_type": "string",
--     "namespace": "global"
--   }
-- ]

-- Create a customer with send_email_invite
-- "send_email_invite": true

-- Create a customer with password and password_confirmation and skip sending the welcome email
-- "password": "newpass",
-- "password_confirmation": "newpass",
-- "send_email_welcome": false

-- Update a customer's marketing opt-in state
-- "marketing_opt_in_level": "confirmed_opt_in"


_getCustomers :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> Maybe CustomerId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Word8 -> Maybe Text -> ClientM Customers
_searchCustomers :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> Maybe Text -> Maybe Word8 -> Maybe Text -> ClientM Customers
_getCustomerById :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> ClientM SingleCustomer
_createCustomer :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> SingleCustomer -> ClientM SingleCustomer
_updateCustomer :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> SingleCustomer -> ClientM SingleCustomer
_createAccountActivationUrl :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> ClientM AccountActivationUrl
_sendAccountInvite :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> SingleCustomerInvite -> ClientM SingleCustomerInvite
_deleteCustomer :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> ClientM DeleteCustomerResponse
_countCustomers :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> ClientM GetCustomerCountResponse
_getOrdersByCustomerId :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> ClientM Orders

( _getCustomers
 :<|> _searchCustomers
 :<|> _getCustomerById
 :<|> _createCustomer
 :<|> _updateCustomer
 :<|> _createAccountActivationUrl
 :<|> _sendAccountInvite
 :<|> _deleteCustomer
 :<|> _countCustomers
 :<|> _getOrdersByCustomerId) = client (Proxy @Api)

