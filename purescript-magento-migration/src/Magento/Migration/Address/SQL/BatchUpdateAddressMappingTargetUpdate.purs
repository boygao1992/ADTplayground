module Magento.Migration.Address.SQL.BatchUpdateAddressMappingTargetUpdate
( batchUpdateAddressMappingTargetUpdate
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (AddressMappingTargetUpdate)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchUpdateAddressMappingTargetUpdate
  :: forall f r
  .  Foldable f
  => Functor f
  => f (AddressMappingTargetUpdate r)
  -> String
_batchUpdateAddressMappingTargetUpdate =
  (\str -> """
INSERT INTO customer_address_entity_mapping
  ( source_address_id
  , target_address_id
  , synced_at
  , last_update
  )
VALUES
""" <> str
    <> """
ON DUPLICATE KEY UPDATE
    target_address_id = VALUES(target_address_id)
  , synced_at         = VALUES(synced_at)
  , last_update       = VALUES(last_update)
""")
  <<< intercalate "\n,"
  <<< map value
  where
    value :: AddressMappingTargetUpdate r -> String
    value =
      F.format (SProxy :: SProxy " ({source_address_id} , {target_address_id} , \"{synced_at}\" , \"{last_update}\")")

batchUpdateAddressMappingTargetUpdate
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f (AddressMappingTargetUpdate r)
  -> Aff Unit
batchUpdateAddressMappingTargetUpdate conn =
  (MySQL.execute_ <@> conn) <<< _batchUpdateAddressMappingTargetUpdate
