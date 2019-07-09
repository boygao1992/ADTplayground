module Magento.Migration.Address.SQL.BatchUpdateAddressMappingTargetInsert
( batchUpdateAddressMappingTargetInsert
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (AddressMappingTargetInsert)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchUpdateAddressMappingTargetInsert
  :: forall f r
  .  Foldable f
  => Functor f
  => f (AddressMappingTargetInsert r)
  -> String
_batchUpdateAddressMappingTargetInsert =
  (\str -> """
INSERT INTO customer_address_entity_mapping
  ( source_address_id
  , target_address_id
  , uuid
  , synced_at
  , last_update
  )
VALUES
""" <> str
    <> """
ON DUPLICATE KEY UPDATE
    target_address_id = VALUES(target_address_id)
  , uuid              = VALUES(uuid)
  , synced_at         = VALUES(synced_at)
  , last_update       = VALUES(last_update)
""")
  <<< intercalate "\n,"
  <<< map value
  where
    value :: AddressMappingTargetInsert r -> String
    value =
      F.format (SProxy :: SProxy " ({source_address_id} , {target_address_id} , \"{uuid}\" , \"{synced_at}\" , \"{last_update}\")")

batchUpdateAddressMappingTargetInsert
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f (AddressMappingTargetInsert r)
  -> Aff Unit
batchUpdateAddressMappingTargetInsert conn =
  (MySQL.execute_ <@> conn) <<< _batchUpdateAddressMappingTargetInsert
