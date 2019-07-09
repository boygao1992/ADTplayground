module Magento.Migration.Grid.SQL.GetSalesFlatShipmentGridEntityIdsToInsert
( getSalesFlatShipmentGridEntityIdsToInsert
) where

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL

_getSalesFlatShipmentGridEntityIdsToInsert :: String
_getSalesFlatShipmentGridEntityIdsToInsert = """
SELECT entity_id
FROM sales_flat_shipment_grid;
"""

getSalesFlatShipmentGridEntityIdsToInsert
  :: MySQL.Connection
  -> Aff (Either String (Array { entity_id :: Int }))
getSalesFlatShipmentGridEntityIdsToInsert =
  MySQL.query'_ _getSalesFlatShipmentGridEntityIdsToInsert
