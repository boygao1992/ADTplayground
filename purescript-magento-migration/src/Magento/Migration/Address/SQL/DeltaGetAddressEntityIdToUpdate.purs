module Magento.Migration.Address.SQL.DeltaGetAddressEntityIdToUpdate
( deltaGetAddressEntityIdToUpdate
) where

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL

_deltaGetAddressEntityIdToUpdate :: String
_deltaGetAddressEntityIdToUpdate =
  """
SELECT am.source_address_id AS source_address_id,
       am.target_address_id AS target_address_id
FROM customer_address_entity AS a
  LEFT JOIN customer_address_entity_mapping AS am
         ON a.entity_id = am.source_address_id
       JOIN customer_entity_mapping AS cm
         ON a.parent_id = cm.source_customer_id
WHERE am.target_address_id IS NOT NULL
  AND a.updated_at > am.last_update
"""

deltaGetAddressEntityIdToUpdate
  :: MySQL.Connection
  -> Aff (Either String (Array { source_address_id :: Int
                               , target_address_id :: Int
                               }))
deltaGetAddressEntityIdToUpdate =
  MySQL.query'_ _deltaGetAddressEntityIdToUpdate
