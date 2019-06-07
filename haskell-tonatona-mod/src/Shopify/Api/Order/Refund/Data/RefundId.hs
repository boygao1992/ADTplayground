{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Shopify.Api.Order.Refund.Data.RefundId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype RefundId = RefundId { unRefundId :: Word32 }
  deriving newtype
    ( Eq, Ord, Show
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
