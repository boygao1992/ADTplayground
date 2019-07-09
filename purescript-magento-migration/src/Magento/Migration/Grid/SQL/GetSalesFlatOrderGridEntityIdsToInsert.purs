module Magento.Migration.Grid.SQL.GetSalesFlatOrderGridEntityIdsToInsert
( getSalesFlatOrderGridEntityIdsToInsert
) where

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL

_getSalesFlatOrderGridEntityIdsToInsert :: String
_getSalesFlatOrderGridEntityIdsToInsert = """
SELECT entity_id
FROM sales_flat_order_grid;
"""

getSalesFlatOrderGridEntityIdsToInsert
  :: MySQL.Connection
  -> Aff (Either String (Array { entity_id :: Int }))
getSalesFlatOrderGridEntityIdsToInsert =
  MySQL.query'_ _getSalesFlatOrderGridEntityIdsToInsert
