{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.CustomerAddress where

import RIO
import Servant
import Data.Aeson.TH

data Address = Address
  { _id :: !(Maybe Word32)
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
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Address)

data Addresses = Addresses
  { _addresses :: ![Address]
  } deriving (Eq, Show)

$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Addresses)
