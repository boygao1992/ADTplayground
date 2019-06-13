module Shopify.Api.InventoryItems.Data.InventoryItemId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype InventoryItemId = InventoryItemId { unInventoryItemId :: Word64 }
  deriving newtype
    ( Eq, Ord
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
  deriving (Show)
