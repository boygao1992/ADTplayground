module Magento.Migration.Grid.SQL.BatchInsertSalesFlatCreditMemoGrid
( batchInsertSalesFlatCreditMemoGrid
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (SalesFlatCreditMemoGrid)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchInsertSalesFlatCreditMemoGrid
  :: forall f
  .  Foldable f
  => Functor f
  => f SalesFlatCreditMemoGrid
  -> String
_batchInsertSalesFlatCreditMemoGrid =
  ("""
INSERT INTO sales_flat_creditmemo_grid
  ( entity_id
  , store_id
  , store_to_order_rate
  , base_to_order_rate
  , grand_total
  , store_to_base_rate
  , base_to_global_rate
  , base_grand_total
  , order_id
  , creditmemo_status
  , state
  , invoice_id
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
    value :: SalesFlatCreditMemoGrid -> String
    value =
      F.format ( SProxy :: SProxy " ({entity_id}, {store_id} , {store_to_order_rate} , {base_to_order_rate} , {grand_total} , {store_to_base_rate} , {base_to_global_rate} , {base_grand_total} , {order_id} , {creditmemo_status} , {state} , {invoice_id} , {store_currency_code} , {order_currency_code} , {base_currency_code} , {global_currency_code} , {increment_id} , {order_increment_id} , {created_at} , {order_created_at} , {billing_name}) ")

batchInsertSalesFlatCreditMemoGrid
  :: forall f
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f SalesFlatCreditMemoGrid
  -> Aff Unit
batchInsertSalesFlatCreditMemoGrid conn =
  (MySQL.execute_ <@> conn) <<< _batchInsertSalesFlatCreditMemoGrid
