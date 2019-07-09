module Magento.Migration.Address.SQL.DeltaGetAddressEntityIdToInsert
( deltaGetAddressEntityIdToInsert
)where

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL

_deltaGetAddressEntityIdToInsert :: String
_deltaGetAddressEntityIdToInsert = """
SELECT a.entity_id as entity_id
FROM customer_address_entity AS a
  LEFT JOIN customer_address_entity_mapping AS am
         ON a.entity_id = am.source_address_id
       JOIN customer_entity_mapping AS cm
         ON a.parent_id = cm.source_customer_id
WHERE am.target_address_id IS NULL
"""

deltaGetAddressEntityIdToInsert
  :: MySQL.Connection
  -> Aff (Either String (Array { entity_id :: Int }))
deltaGetAddressEntityIdToInsert =
  MySQL.query'_ _deltaGetAddressEntityIdToInsert
