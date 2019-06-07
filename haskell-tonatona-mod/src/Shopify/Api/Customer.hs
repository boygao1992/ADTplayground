{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Customer where

-- TODO implement ad-hoc capture group to handle .json suffix gracefully

import RIO
import Servant
import Servant.Client (client)

import qualified Shopify.Api.Customer.Address as CustomerAddress
import Shopify.Api.Order (Orders)
import Shopify.Api.Customer.Data.Customer
import Shopify.Api.Customer.Data.CustomerId (CustomerId)

type Api = "customers" :> CustomersApi
type CustomersApi
  = QueryParam "ids" Text -- TODO implement ToHTTPApiData for CustomerIds
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
  :<|> QueryParam "order" Text
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

  :<|> Capture "customer_id" CustomerId
    :> Get '[JSON] SingleCustomer
    -- GET /admin/api/2019-04/customers/#{customer_id}.json
    -- Retrieves a single customer

  :<|> ReqBody '[JSON] SingleCustomer
    :> Post '[JSON] SingleCustomer
    -- POST /admin/api/2019-04/customers.json
    -- Creates a customer

  :<|> Capture "customer_id" CustomerId
    :> ReqBody '[JSON] SingleCustomer
    :> Put '[JSON] SingleCustomer
    -- PUT /admin/api/2019-04/customers/#{customer_id}.json
    -- Updates a customer

  :<|> Capture "customer_id" CustomerId
    :> "account_activation_url"
    :> Post '[JSON] AccountActivationUrl
    -- POST /admin/api/2019-04/customers/#{customer_id}/account_activation_url.json
    -- Creates an account activation URL for a customer

  :<|> Capture "customer_id" CustomerId
    :> "send_invite"
    :> ReqBody '[JSON] SingleCustomerInvite
    :> Post '[JSON] SingleCustomerInvite
    -- POST /admin/api/2019-04/customers/#{customer_id}/send_invite.json
    -- Sends an account invite to a customer

  :<|> Capture "customer_id" CustomerId
    :> Delete '[JSON] DeleteCustomerResponse
    -- DELETE /admin/api/2019-04/customers/#{customer_id}.json
    -- Deletes a customer.

  :<|> Capture "customer_id" CustomerId
    :> "count"
    :> Get '[JSON] GetCustomerCountResponse
    -- GET /admin/api/2019-04/customers/count.json
    -- Retrieves a count of customers

  :<|> Capture "customer_id" CustomerId
    :> "orders"
    :> Get '[JSON] Orders
    -- GET /admin/api/2019-04/customers/#{customer_id}/orders.json
    -- Retrieves all orders belonging to a customer

  :<|> Capture "customer_id" CustomerId
    :> CustomerAddress.Api

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

( getCustomers
 :<|> searchCustomers
 :<|> getCustomerById
 :<|> createCustomer
 :<|> updateCustomer
 :<|> createAccountActivationUrl
 :<|> sendAccountInvite
 :<|> deleteCustomer
 :<|> countCustomers
 :<|> getOrdersByCustomerId
 :<|> addressApi) = client (Proxy @Api)
