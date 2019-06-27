module Shopify.Data.Orders.Fulfillments.ShipmentStatus where

import RIO
import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toJSON)

data ShipmentStatus
  = Label_Printed
  | Label_Purchased
  | Attempted_Delivery
  | Ready_For_Pickup
  | Confirmed
  | In_Transit
  | Out_For_Delivery
  | Delivered
  | Failure
  deriving (Eq, Ord, Show, Enum)

instance ToJSON ShipmentStatus where
  toJSON Label_Printed      = "label_printed"
  toJSON Label_Purchased    = "label_purchased"
  toJSON Attempted_Delivery = "attempted_delivery"
  toJSON Ready_For_Pickup   = "ready_for_pickup"
  toJSON Confirmed          = "confirmed"
  toJSON In_Transit         = "in_transit"
  toJSON Out_For_Delivery   = "out_for_delivery"
  toJSON Delivered          = "delivered"
  toJSON Failure            = "failure"

instance FromJSON ShipmentStatus where
  parseJSON (String "label_printed"      ) = pure Label_Printed
  parseJSON (String "label_purchased"    ) = pure Label_Purchased
  parseJSON (String "attempted_delivery" ) = pure Attempted_Delivery
  parseJSON (String "ready_for_pickup"   ) = pure Ready_For_Pickup
  parseJSON (String "confirmed"          ) = pure Confirmed
  parseJSON (String "in_transit"         ) = pure In_Transit
  parseJSON (String "out_for_delivery"   ) = pure Out_For_Delivery
  parseJSON (String "delivered"          ) = pure Delivered
  parseJSON (String "failure"            ) = pure Failure
  parseJSON _ = fail "Unrecoginzed shipment status"
