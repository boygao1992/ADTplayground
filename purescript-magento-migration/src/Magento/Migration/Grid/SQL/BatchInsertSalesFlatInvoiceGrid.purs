module Magento.Migration.Grid.SQL.BatchInsertSalesFlatInvoiceGrid
( batchInsertSalesFlatInvoiceGrid
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (SalesFlatInvoiceGrid)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchInsertSalesFlatInvoiceGrid
  :: forall f
  .  Foldable f
  => Functor f
  => f SalesFlatInvoiceGrid
  -> String
_batchInsertSalesFlatInvoiceGrid =
  ("""
INSERT INTO sales_flat_invoice_grid
  ( entity_id
  , store_id
  , base_grand_total
  , grand_total
  , order_id
  , state
  , store_currency_code
  , order_currency_code
  , base_currency_code
  , global_currency_code
  , increment_id
  , order_increment_id
  , created_at
  , order_created_at
  , billing_name
  )
VALUES
""" <> _)
  <<< intercalate "\n,"
  <<< map value
  where
    value :: SalesFlatInvoiceGrid -> String
    value =
      F.format ( SProxy :: SProxy " ({entity_id}, {store_id} , {base_grand_total} , {grand_total} , {order_id} , {state} , {store_currency_code} , {order_currency_code} , {base_currency_code} , {global_currency_code} , {increment_id} , {order_increment_id} , {created_at} , {order_created_at} , {billing_name})")

batchInsertSalesFlatInvoiceGrid
  :: forall f
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f SalesFlatInvoiceGrid
  -> Aff Unit
batchInsertSalesFlatInvoiceGrid conn =
  (MySQL.execute_ <@> conn) <<< _batchInsertSalesFlatInvoiceGrid
