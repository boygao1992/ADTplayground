module Magento.Migration.Grid.SQL.GetSalesFlatCreditMemoGridEntityIdsToInsert
( getSalesFlatCreditMemoGridEntityIdsToInsert
) where

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL

_getSalesFlatCreditMemoGridEntityIdsToInsert :: String
_getSalesFlatCreditMemoGridEntityIdsToInsert = """
SELECT entity_id
FROM sales_flat_creditmemo_grid;
"""

getSalesFlatCreditMemoGridEntityIdsToInsert
  :: MySQL.Connection
  -> Aff (Either String (Array { entity_id :: Int }))
getSalesFlatCreditMemoGridEntityIdsToInsert =
  MySQL.query'_ _getSalesFlatCreditMemoGridEntityIdsToInsert
