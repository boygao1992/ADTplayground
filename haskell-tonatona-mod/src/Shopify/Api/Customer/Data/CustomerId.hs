module Shopify.Api.Customer.Data.CustomerId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype CustomerId = CustomerId { unCustomerId :: Word64 }
  deriving newtype
    ( Eq, Ord
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
  deriving (Show)
