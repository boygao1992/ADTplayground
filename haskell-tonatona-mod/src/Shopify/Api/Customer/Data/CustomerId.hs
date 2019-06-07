module Shopify.Api.Customer.Data.CustomerId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype CustomerId = CustomerId { unCustomerId :: Word32 }
  deriving newtype
    ( Eq, Show, Ord
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
