module Magento.Migration.Grid.SQL.BatchInsertSalesFlatShipmentGrid
( batchInsertSalesFlatShipmentGrid
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (SalesFlatShipmentGrid)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchInsertSalesFlatShipmentGrid
  :: forall f
  .  Foldable f
  => Functor f
  => f SalesFlatShipmentGrid
  -> String
_batchInsertSalesFlatShipmentGrid =
  ("""
INSERT INTO sales_flat_shipment_grid
  ( entity_id
  , store_id
  , total_qty
  , order_id
  , shipment_status
  , increment_id
  , order_increment_id
  , created_at
  , order_created_at
  , shipping_name
  )
VALUES
""" <> _)
  <<< intercalate "\n,"
  <<< map value
  where
    value :: SalesFlatShipmentGrid -> String
    value =
      F.format ( SProxy :: SProxy " ({entity_id}, {store_id} , {total_qty} , {order_id} , {shipment_status} , {increment_id} , {order_increment_id} , {created_at} , {order_created_at} , {shipping_name})")

batchInsertSalesFlatShipmentGrid
  :: forall f
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f SalesFlatShipmentGrid
  -> Aff Unit
batchInsertSalesFlatShipmentGrid conn =
  (MySQL.execute_ <@> conn) <<< _batchInsertSalesFlatShipmentGrid
