module Shopify.Api.Order.Fulfillment.Data.FulfillmentId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype FulfillmentId = FulfillmentId { unFulfillmentId :: Word32}
  deriving newtype
    ( Eq, Ord
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
  deriving (Show)
