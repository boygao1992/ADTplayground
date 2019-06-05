{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Customer where

-- TODO implement ad-hoc capture group to handle .json suffix gracefully

import RIO
import Servant
import Data.Aeson.TH

import Shopify.Api.CustomerAddress

data Customer = Customer
  { _id :: !(Maybe Word32)
    -- "id": 207119551,
  , _email :: !(Maybe Text)
    -- "email": "bob.norman@hostmail.com",
  , _accepts_marketing :: !(Maybe Bool)
    -- "accepts_marketing": false,
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-04-18T15:41:45-04:00",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2019-04-18T15:41:45-04:00",
  , _first_name :: !(Maybe Text)
    -- "first_name": "Bob",
  , _last_name :: !(Maybe Text)
    -- "last_name": "Norman",
  , _orders_count :: !(Maybe Word32)
    -- "orders_count": 1,
  , _state :: !(Maybe Text)
    -- "state": "disabled",
  , _total_spent :: !(Maybe Text)
    -- "total_spent": "199.65",
  , _last_order_id :: !(Maybe Word32)
    -- "last_order_id": 450789469,
  , _note :: !(Maybe Text)
    -- "note": null,
  , _verified_email :: !(Maybe Bool)
    -- "verified_email": true,
  , _multipass_identifier :: !(Maybe Text)
    -- "multipass_identifier": null,
  , _tax_exempt :: !(Maybe Bool)
    -- "tax_exempt": false,
  , _phone :: !(Maybe Text)
    -- "phone": null,
  , _tags :: !(Maybe Text) -- NOTE a string of comma-separated values
    -- "tags": "",
  , _last_order_name :: !(Maybe Text)
    -- "last_order_name": "#1001",
  , _currency :: !(Maybe Text)
    -- "currency": "USD",
  , _accepts_marketing_updated_at :: !(Maybe Text)
    -- "accepts_marketing_updated_at": "2005-06-12T11:57:11-04:00",
  , _marketing_opt_in_level :: !(Maybe Text)
    -- "marketing_opt_in_level": null,
  , _admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Customer/207119551",
  , _addresses :: !(Maybe [Address])
    -- "addresses": [{}]
  , _default_address :: !(Maybe Address)
    -- "default_address": {}
  } deriving (Eq, Show)

$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Customer)

data Customers = Customers
  { _customers :: ![Customer]
  } deriving (Eq, Show)

$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Customers)


type Api
  = "customers.json"
    :> QueryParam "ids"
    -- Restrict results to customers specified by a comma-separated list of IDs.
    :> QueryParam "since_id"
    -- Restrict results to those after the specified ID.
    :> QueryParam "created_at_min"
    -- Show customers created after a specified date.
    -- (format: 2014-04-25T16:15:47-04:00)
    :> QueryParam "created_at_max"
    -- Show customers created before a specified date.
    -- (format: 2014-04-25T16:15:47-04:00)
    :> QueryParam "updated_at_min"
    -- Show customers last updated after a specified date.
    -- (format: 2014-04-25T16:15:47-04:00)
    :> QueryParam "updated_at_max"
    -- Show customers last updated before a specified date.
    -- (format: 2014-04-25T16:15:47-04:00)
    :> QueryParam "limit"
    -- The maximum number of results to show.
    -- (default: 50, maximum: 250)
    :> QueryParam "fields"
    -- Show only certain fields, specified by a comma-separated list of field names.
    :> Get '[JSON] Customers
  :<|> "search.json"
    :> QueryParam "order"
    -- Set the field and direction by which to order results.
    -- (default: last_order_date DESC)
    :> QueryParam "query"
    -- Text to search for in the shop's customer data.
    :> QueryParam "limit"
    -- The maximum number of results to show.
    -- (default: 50, maximum: 250)
    :> QueryParam "fields"
    -- Show only certain fields, specified by a comma-separated list of field names.
    :> Get '[JSON] Customers
  :<|> "customers"
    :> Capture "customer_id" Text -- NOTE manually strip ".json" -> Word32
    :> Get '[JSON] Customer
  :<|> "customers"
    :> ReqBody '[JSON] Customer
    :> Post '[JSON] Customer
  :<|> "customers"
    :> Capture "customer_id" Text -- NOTE manually strip ".json" -> Word32
    :> ReqBody '[JSON] Customer
    :> Put '[JSON] Customer

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
