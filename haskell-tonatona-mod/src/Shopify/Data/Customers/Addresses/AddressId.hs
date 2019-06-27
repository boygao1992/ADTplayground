module Shopify.Data.Customers.Addresses.AddressId where

import RIO
import Servant
import Data.Aeson (FromJSON, ToJSON)

newtype AddressId = AddressId { unAddressId :: Word64 }
  deriving newtype
    ( Eq, Ord
    , ToHttpApiData, FromHttpApiData
    , FromJSON, ToJSON
    )
  deriving (Show)
