module Shopify.Api.Customers.Addresses.Req.Data.AddressOperation where

import RIO
import Servant

data AddressOperation
  = AddressDestroy
  deriving (Eq, Ord, Show)
instance ToHttpApiData AddressOperation where
  toQueryParam AddressDestroy = "destroy"
