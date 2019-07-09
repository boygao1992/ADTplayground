module Magento.Migration.Address.SQL.FullInsertAddressMappingSource
( fullInsertAddressMappingSource
) where

import Prelude

import Effect.Aff (Aff)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_fullInsertAddressMappingSource
  :: String
  -> String
_fullInsertAddressMappingSource created_at =
  F.format
    ( SProxy :: SProxy """
INSERT IGNORE INTO customer_address_entity_mapping
  (source_address_id, source_customer_id, created_at)
SELECT
    a.entity_id AS source_address_id
  , cm.source_customer_id AS source_customer_id
  , "{created_at}" AS created_at
FROM customer_address_entity AS a
  JOIN customer_entity_mapping AS cm
    ON a.parent_id = cm.source_customer_id
""")
    { created_at }

fullInsertAddressMappingSource
  :: MySQL.Connection
  -> String
  -> Aff Unit
fullInsertAddressMappingSource conn =
  (MySQL.execute_ <@> conn) <<< _fullInsertAddressMappingSource
