module Shopify.Api.Order.Data.OrderId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype OrderId = OrderId { unOrderId :: Word32}
  deriving newtype
    ( Eq, Show, Ord
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
