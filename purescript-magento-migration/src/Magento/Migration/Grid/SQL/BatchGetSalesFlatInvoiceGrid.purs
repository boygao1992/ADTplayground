module Magento.Migration.Grid.SQL.BatchGetSalesFlatInvoiceGrid
( batchGetSalesFlatInvoiceGrid
) where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (SalesFlatInvoiceGrid)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchGetSalesFlatInvoiceGrid
  :: forall f r
  .  Foldable f
  => Functor f
  => f { entity_id :: Int | r }
  -> String
_batchGetSalesFlatInvoiceGrid =
  ("""
SELECT
    entity_id
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
FROM sales_flat_invoice_grid
WHERE""" <> _)
  <<< intercalate "\nOR"
  <<< map condition
  where
    condition :: { entity_id :: Int | r } -> String
    condition = F.format ( SProxy :: SProxy " entity_id=\"{entity_id}\" ")


batchGetSalesFlatInvoiceGrid
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f { entity_id :: Int | r }
  -> Aff (Either String (Array SalesFlatInvoiceGrid))
batchGetSalesFlatInvoiceGrid conn =
  (MySQL.query'_ <@> conn) <<< _batchGetSalesFlatInvoiceGrid
