module Shopify.Api.Order.Fulfillment.Data.Status where

import RIO
import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toJSON)

data Status
  = Pending
  | Open
  | Success
  | Cancelled
  | Error
  | Failure
  deriving (Eq, Ord, Show, Enum)

instance ToJSON Status where
  toJSON Pending   = "pending"
  toJSON Open      = "open"
  toJSON Success   = "success"
  toJSON Cancelled = "cancelled"
  toJSON Error     = "error"
  toJSON Failure   = "failure"

instance FromJSON Status where
  parseJSON (String "pending"  ) = pure Pending
  parseJSON (String "open"     ) = pure Open
  parseJSON (String "success"  ) = pure Success
  parseJSON (String "cancelled") = pure Cancelled
  parseJSON (String "error"    ) = pure Error
  parseJSON (String "failure"  ) = pure Failure
  parseJSON _ = fail "Unrecoginzed fulfillment status"
