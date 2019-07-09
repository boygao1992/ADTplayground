module Magento.Migration.Grid.SQL.BatchGetSalesFlatOrderGrid
( batchGetSalesFlatOrderGrid
) where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (SalesFlatOrderGrid)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchGetSalesFlatOrderGrid
  :: forall f r
  .  Foldable f
  => Functor f
  => f { entity_id :: Int | r }
  -> String
_batchGetSalesFlatOrderGrid =
  ("""
SELECT
    entity_id
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
FROM sales_flat_order_grid
WHERE""" <> _)
  <<< intercalate "\nOR"
  <<< map condition
  where
    condition :: { entity_id :: Int | r } -> String
    condition = F.format ( SProxy :: SProxy " entity_id=\"{entity_id}\" ")


batchGetSalesFlatOrderGrid
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f { entity_id :: Int | r }
  -> Aff (Either String (Array SalesFlatOrderGrid))
batchGetSalesFlatOrderGrid conn =
  (MySQL.query'_ <@> conn) <<< _batchGetSalesFlatOrderGrid
