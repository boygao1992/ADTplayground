{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Customers.Addresses.Address where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Customers.CustomerId (CustomerId)
import Shopify.Data.Customers.Addresses.AddressId (AddressId)

data Address = Address
  { __id :: !(Maybe AddressId)
    -- "id": 207119551,
  , __customer_id :: !(Maybe CustomerId)
    -- "customer_id": 207119551,
  , __first_name :: !(Maybe Text)
    -- "first_name": null,
  , __last_name :: !(Maybe Text)
    -- "last_name": null,
  , __company :: !(Maybe Text)
    -- "company": null,
  , __address1 :: !(Maybe Text)
    -- "address1": "Chestnut Street 92",
  , __address2 :: !(Maybe Text)
    -- "address2": "",
  , __city :: !(Maybe Text)
    -- "city": "Louisville",
  , __province :: !(Maybe Text)
    -- "province": "Kentucky",
  , __country :: !(Maybe Text)
    -- "country": "United States",
  , __zip :: !(Maybe Text)
    -- "zip": "40202",
  , __phone :: !(Maybe Text)
    -- "phone": "555-625-1199",
  , __name :: !(Maybe Text)
    -- "name": "",
  , __province_code :: !(Maybe Text)
    -- "province_code": "KY",
  , __country_code :: !(Maybe Text)
    -- "country_code": "US",
  , __country_name :: !(Maybe Text)
    -- "country_name": "United States",
  , __default :: !(Maybe Bool)
    -- "default": true
  , __latitude :: !(Maybe Text)
    -- "latitude": "45.41634", -- NOTE business/shipping address only
  , __longitude :: !(Maybe Text)
    -- "longitude": "-75.6868", -- NOTE business/shipping address only
  } deriving (Eq, Show)
$(makeLensesDropOne ''Address)
$(deriveJSONDropTwo ''Address)

data SingleAddress = SingleAddress
  { __customer_address :: !Address
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''SingleAddress)

data Addresses = Addresses
  { __addresses :: ![Address]
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''Addresses)
