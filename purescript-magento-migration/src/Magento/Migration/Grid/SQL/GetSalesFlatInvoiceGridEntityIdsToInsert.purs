module Magento.Migration.Grid.SQL.GetSalesFlatInvoiceGridEntityIdsToInsert
( getSalesFlatInvoiceGridEntityIdsToInsert
) where

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL

_getSalesFlatInvoiceGridEntityIdsToInsert :: String
_getSalesFlatInvoiceGridEntityIdsToInsert = """
SELECT entity_id
FROM sales_flat_invoice_grid;
"""

getSalesFlatInvoiceGridEntityIdsToInsert
  :: MySQL.Connection
  -> Aff (Either String (Array { entity_id :: Int }))
getSalesFlatInvoiceGridEntityIdsToInsert =
  MySQL.query'_ _getSalesFlatInvoiceGridEntityIdsToInsert
