{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Customers.Customer where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

import Shopify.Data.Customers.Addresses.Address as CustomerAddress
import Shopify.Data.Customers.CustomerId (CustomerId)

data Customer = Customer
  { __id :: !(Maybe CustomerId)
    -- "id": 207119551,
  , __email :: !(Maybe Text)
    -- "email": "bob.norman@hostmail.com",
  , __accepts_marketing :: !(Maybe Bool)
    -- "accepts_marketing": false,
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-04-18T15:41:45-04:00",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2019-04-18T15:41:45-04:00",
  , __first_name :: !(Maybe Text)
    -- "first_name": "Bob",
  , __last_name :: !(Maybe Text)
    -- "last_name": "Norman",
  , __orders_count :: !(Maybe Word64)
    -- "orders_count": 1,
  , __state :: !(Maybe Text)
    -- "state": "disabled",
  , __total_spent :: !(Maybe Text)
    -- "total_spent": "199.65",
  , __last_order_id :: !(Maybe Word64)
    -- "last_order_id": 450789469,
  , __note :: !(Maybe Text)
    -- "note": null,
  , __verified_email :: !(Maybe Bool)
    -- "verified_email": true,
  , __multipass_identifier :: !(Maybe Text)
    -- "multipass_identifier": null,
  , __tax_exempt :: !(Maybe Bool)
    -- "tax_exempt": false,
  , __phone :: !(Maybe Text)
    -- "phone": null,
  , __tags :: !(Maybe Text) -- NOTE a string of comma-separated values
    -- "tags": "",
  , __last_order_name :: !(Maybe Text)
    -- "last_order_name": "#1001",
  , __currency :: !(Maybe Text)
    -- "currency": "USD",
  , __accepts_marketing_updated_at :: !(Maybe Text)
    -- "accepts_marketing_updated_at": "2005-06-12T11:57:11-04:00",
  , __marketing_opt_in_level :: !(Maybe Text)
    -- "marketing_opt_in_level": null,
  , __admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Customer/207119551",
  , __addresses :: !(Maybe [Address])
    -- "addresses": [{}]
  , __default_address :: !(Maybe Address)
    -- "default_address": {}
  } deriving (Eq, Show)
$(makeLensesDropOne ''Customer)
$(deriveJSONDropTwo ''Customer)

data SingleCustomer = SingleCustomer
  { __customer :: !Customer
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''SingleCustomer)

data Customers = Customers
  { __customers :: ![Customer]
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''Customers)


