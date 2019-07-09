{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Customers where

import RIO
import Servant
import Servant.Client.Core (AuthenticatedRequest)

import Shopify.Api.Admin.OAuth.Data.AccessToken (accessTokenL)
import Shopify.Api.Customers.Req.CreateAccountActivationUrl as CreateAccountActivationUrl (Res)
import Shopify.Api.Customers.Req.CountCustomers as CountCustomers (Res)
import Shopify.Api.Customers.Req.GetCustomers as GetCustomers (Req(..))
import Shopify.Api.Customers.Req.SearchCustomers as SearchCustomers (Req(..))
import Shopify.Api.Customers.Req.SendAccountInvite (SingleCustomerInvite)
import Shopify.Api.Customers.Req.DeleteCustomer as DeleteCustomer (Res)
import Shopify.Data.Customers.Customer
import Shopify.Data.Customers.CustomerId (CustomerId)
import Shopify.Data.Orders.Order (Orders)
import Shopify.Servant.DotJSON (DotJSON)
import Shopify.Servant.Client.Util (getApiClients, mkShopifyAuthenticateReq)
import Shopify.TestApp.Types (ApiHttpClientResources)

type Api
  = GetCustomers
  :<|> SearchCustomers
  :<|> GetCustomerById
  :<|> CreateCustomer
  :<|> UpdateCustomer
  :<|> DeleteCustomer
  :<|> CreateAccountActivationUrl
  :<|> SendAccountInvite
  :<|> CountCustomers
  :<|> GetOrdersByCustomerId

_getCustomers
  :<|> _searchCustomers
  :<|> _getCustomerById
  :<|> _createCustomer
  :<|> _updateCustomer
  :<|> _deleteCustomer
  :<|> _createAccountActivationUrl
  :<|> _sendAccountInvite
  :<|> _countCustomers
  :<|> _getOrdersByCustomerId
  = getApiClients (Proxy @Api)

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

----------------
-- Type Synonyms

-- /customers
type CustomersApi a
  = AuthProtect "shopify-access-token"
  :> "customers"
  :> a

-- /customers/#{customer_id}
type SingleCustomerApi a
  = CustomersApi
  ( Capture "customer_id" CustomerId
  :> a
  )

---------------
-- getCustomers

type GetCustomers
  = CustomersApi
  ( DotJSON
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
  )
  -- GET /admin/api/2019-04/customers.json
  -- Retrieves a list of customers

getCustomers :: GetCustomers.Req -> RIO ApiHttpClientResources Customers
getCustomers (GetCustomers.Req _ids _since_id _created_at_min _created_at_max _updated_at_min _updated_at_max _limit _fields) = do
  token <- view accessTokenL
  _getCustomers (mkShopifyAuthenticateReq token)
      _ids _since_id _created_at_min _created_at_max _updated_at_min _updated_at_max _limit _fields

_getCustomers :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> Maybe CustomerId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Word8 -> Maybe Text -> RIO ApiHttpClientResources Customers

------------------
-- searchCustomers

type SearchCustomers
  = CustomersApi
  ( "search" :> DotJSON
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
  )
  -- GET /admin/api/2019-04/customers/search.json?query=Bob country:United States
  -- Searches for customers that match a supplied query

searchCustomers :: SearchCustomers.Req -> RIO ApiHttpClientResources Customers
searchCustomers (SearchCustomers.Req _order _query _limit _fields) = do
  token <- view accessTokenL
  _searchCustomers (mkShopifyAuthenticateReq token)
      _order _query _limit _fields

_searchCustomers :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> Maybe Text -> Maybe Text -> Maybe Word8 -> Maybe Text -> RIO ApiHttpClientResources Customers

------------------
-- getCustomerById

type GetCustomerById
  = SingleCustomerApi
  ( DotJSON
  :> Get '[JSON] SingleCustomer
  )
  -- GET /admin/api/2019-04/customers/#{customer_id}.json
  -- Retrieves a single customer

getCustomerById :: CustomerId -> RIO ApiHttpClientResources SingleCustomer
getCustomerById cid = do
  token <- view accessTokenL
  _getCustomerById (mkShopifyAuthenticateReq token) cid

_getCustomerById :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> RIO ApiHttpClientResources SingleCustomer

-----------------
-- createCustomer

type CreateCustomer
  = CustomersApi
  ( DotJSON
  :> ReqBody '[JSON] SingleCustomer
  :> Post '[JSON] SingleCustomer
  )
  -- POST /admin/api/2019-04/customers.json
  -- Creates a customer

createCustomer :: SingleCustomer -> RIO ApiHttpClientResources SingleCustomer
createCustomer c = do
  token <- view accessTokenL
  _createCustomer (mkShopifyAuthenticateReq token) c

_createCustomer :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> SingleCustomer -> RIO ApiHttpClientResources SingleCustomer

-----------------
-- updateCustomer

type UpdateCustomer
  = SingleCustomerApi
  ( DotJSON
  :> ReqBody '[JSON] SingleCustomer
  :> Put '[JSON] SingleCustomer
  )
  -- PUT /admin/api/2019-04/customers/#{customer_id}.json
  -- Updates a customer

updateCustomer :: CustomerId -> SingleCustomer -> RIO ApiHttpClientResources SingleCustomer
updateCustomer cid c = do
  token <- view accessTokenL
  _updateCustomer (mkShopifyAuthenticateReq token) cid c

_updateCustomer :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> SingleCustomer -> RIO ApiHttpClientResources SingleCustomer

-----------------
-- deleteCustomer

type DeleteCustomer
  = SingleCustomerApi
  ( DotJSON
  :> Delete '[JSON] DeleteCustomer.Res
  )
  -- DELETE /admin/api/2019-04/customers/#{customer_id}.json
  -- Deletes a customer.

deleteCustomer :: CustomerId -> RIO ApiHttpClientResources DeleteCustomer.Res
deleteCustomer cid = do
  token <- view accessTokenL
  _deleteCustomer (mkShopifyAuthenticateReq token) cid

_deleteCustomer :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> RIO ApiHttpClientResources DeleteCustomer.Res

-----------------------------
-- createAccountActivationUrl

type CreateAccountActivationUrl
  = SingleCustomerApi
  ( "account_activation_url" :> DotJSON
  :> Post '[JSON] CreateAccountActivationUrl.Res
  )
  -- POST /admin/api/2019-04/customers/#{customer_id}/account_activation_url.json
  -- Creates an account activation URL for a customer

createAccountActivationUrl :: CustomerId -> RIO ApiHttpClientResources CreateAccountActivationUrl.Res
createAccountActivationUrl cid = do
  token <- view accessTokenL
  _createAccountActivationUrl (mkShopifyAuthenticateReq token) cid

_createAccountActivationUrl :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> RIO ApiHttpClientResources CreateAccountActivationUrl.Res

--------------------
-- sendAccountInvite

type SendAccountInvite
  = SingleCustomerApi
  ( "send_invite" :> DotJSON
  :> ReqBody '[JSON] SingleCustomerInvite
  :> Post '[JSON] SingleCustomerInvite
  )
  -- POST /admin/api/2019-04/customers/#{customer_id}/send_invite.json
  -- Sends an account invite to a customer

sendAccountInvite :: CustomerId -> SingleCustomerInvite -> RIO ApiHttpClientResources SingleCustomerInvite
sendAccountInvite cid s = do
  token <- view accessTokenL
  _sendAccountInvite (mkShopifyAuthenticateReq token) cid s

_sendAccountInvite :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> SingleCustomerInvite -> RIO ApiHttpClientResources SingleCustomerInvite

-----------------
-- countCustomers

type CountCustomers
  = SingleCustomerApi
  ( "count" :> DotJSON
  :> Get '[JSON] CountCustomers.Res
  )
  -- GET /admin/api/2019-04/customers/count.json
  -- Retrieves a count of customers

countCustomers :: CustomerId -> RIO ApiHttpClientResources CountCustomers.Res
countCustomers cid = do
  token <- view accessTokenL
  _countCustomers (mkShopifyAuthenticateReq token) cid

_countCustomers :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> RIO ApiHttpClientResources CountCustomers.Res

------------------------
-- getOrdersByCustomerId

type GetOrdersByCustomerId
  = SingleCustomerApi
  ( "orders" :> DotJSON
  :> Get '[JSON] Orders
  )
  -- GET /admin/api/2019-04/customers/#{customer_id}/orders.json
  -- Retrieves all orders belonging to a customer

getOrdersByCustomerId :: CustomerId -> RIO ApiHttpClientResources Orders
getOrdersByCustomerId cid = do
  token <- view accessTokenL
  _getOrdersByCustomerId (mkShopifyAuthenticateReq token) cid

_getOrdersByCustomerId :: AuthenticatedRequest (AuthProtect "shopify-access-token") -> CustomerId -> RIO ApiHttpClientResources Orders

