module Magento.Migration.Customer.SQL.BatchUpdateCustomerMappingTarget
( batchUpdateCustomerMappingTarget
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (CustomerMappingTarget)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))


_batchUpdateCustomerMappingTarget
  :: forall f r
  .  Foldable f
  => Functor f
  => f (CustomerMappingTarget r)
  -> String
_batchUpdateCustomerMappingTarget =
  (\str ->
    """
INSERT INTO
  customer_entity_mapping
    ( source_customer_id
    , target_customer_id
    , synced_at
    , last_update
    )
VALUES
"""
    <> str
    <> """
ON DUPLICATE KEY UPDATE
    target_customer_id = VALUES(target_customer_id)
  , synced_at = VALUES(synced_at)
  , last_update = VALUES(last_update)
""")
  <<< intercalate "\n,"
  <<< map _value
  where
    _value
      :: { source_customer_id :: Int
         , target_customer_id :: Int
         , synced_at :: String
         , last_update :: String
         | r
         }
      -> String
    _value =
      F.format
        ( SProxy :: SProxy "({source_customer_id}, {target_customer_id}, \"{synced_at}\", \"{last_update}\")")



batchUpdateCustomerMappingTarget
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f (CustomerMappingTarget r)
  -> Aff Unit
batchUpdateCustomerMappingTarget conn =
  (MySQL.execute_ <@> conn) <<< _batchUpdateCustomerMappingTarget

