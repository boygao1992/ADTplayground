{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Customers.Addresses.Address where

import RIO
import Data.Aeson.TH
import Shopify.Data.Customers.CustomerId (CustomerId)
import Shopify.Data.Customers.Addresses.AddressId (AddressId)

data Address = Address
  { _id :: !(Maybe AddressId)
    -- "id": 207119551,
  , _customer_id :: !(Maybe CustomerId)
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
