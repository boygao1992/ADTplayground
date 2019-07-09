module Magento.Migration.Grid.SQL.BatchInsertSalesFlatOrderGrid
( batchInsertSalesFlatOrderGrid
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (SalesFlatOrderGrid)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchInsertSalesFlatOrderGrid
  :: forall f
  .  Foldable f
  => Functor f
  => f SalesFlatOrderGrid
  -> String
_batchInsertSalesFlatOrderGrid =
  ("""
INSERT INTO sales_flat_order_grid
  ( entity_id
  , status
  , store_id
  , store_name
  , customer_id
  , base_grand_total
  , base_total_paid
  , grand_total
  , total_paid
  , increment_id
  , base_currency_code
  , order_currency_code
  , shipping_name
  , billing_name
  , created_at
  , updated_at
  )
VALUES
""" <> _)
  <<< intercalate "\n,"
  <<< map value
  where
    value :: SalesFlatOrderGrid -> String
    value =
      F.format ( SProxy :: SProxy " ({entity_id}, {status} , {store_id} , {store_name} , {customer_id} , {base_grand_total} , {base_total_paid} , {grand_total} , {total_paid} , {increment_id} , {base_currency_code} , {order_currency_code} , {shipping_name} , {billing_name} , {created_at} , {updated_at})")

batchInsertSalesFlatOrderGrid
  :: forall f
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f SalesFlatOrderGrid
  -> Aff Unit
batchInsertSalesFlatOrderGrid conn =
  (MySQL.execute_ <@> conn) <<< _batchInsertSalesFlatOrderGrid
