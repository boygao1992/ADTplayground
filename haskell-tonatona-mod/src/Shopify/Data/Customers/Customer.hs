{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Customers.Customer where

import RIO
import Data.Aeson.TH

import Shopify.Data.Customers.Addresses.Address as CustomerAddress
import Shopify.Data.Customers.CustomerId (CustomerId)


data Customer = Customer
  { _id :: !(Maybe CustomerId)
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
  , _orders_count :: !(Maybe Word64)
    -- "orders_count": 1,
  , _state :: !(Maybe Text)
    -- "state": "disabled",
  , _total_spent :: !(Maybe Text)
    -- "total_spent": "199.65",
  , _last_order_id :: !(Maybe Word64)
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
      , omitNothingFields = True
      }
    ''Customer)

data SingleCustomer = SingleCustomer
  { _customer :: !Customer
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''SingleCustomer)

data Customers = Customers
  { _customers :: ![Customer]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Customers)

data AccountActivationUrl = AccountActivationUrl
  { _account_activation_url :: !(Maybe Text)
  , _errors :: !(Maybe [Text])
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''AccountActivationUrl)

data CustomerInvite = CustomerInvite
  { _to :: !(Maybe Text)
  , _from :: !(Maybe Text)
  , _subject :: !(Maybe Text)
  , _custom_message :: !(Maybe Text)
  , _bcc :: !(Maybe [Text])
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''CustomerInvite)

data SingleCustomerInvite = SingleCustomerInvite
  { _customer_invite :: !CustomerInvite
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''SingleCustomerInvite)

data DeleteCustomerResponse = DeleteCustomerResponse
  { __errors :: !(Maybe Text)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 2
      , omitNothingFields = True
      }
    ''DeleteCustomerResponse)

data GetCustomerCountResponse = GetCustomerCountResponse
  { _count :: !(Maybe Word64)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''GetCustomerCountResponse)


