module Magento.Migration.Grid.SQL.BatchGetSalesFlatCreditMemoGrid
( batchGetSalesFlatCreditMemoGrid
) where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (SalesFlatCreditMemoGrid)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchGetSalesFlatCreditMemoGrid
  :: forall f r
  .  Foldable f
  => Functor f
  => f { entity_id :: Int | r }
  -> String
_batchGetSalesFlatCreditMemoGrid =
  ("""
SELECT
    entity_id
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
FROM sales_flat_creditmemo_grid
WHERE""" <> _)
  <<< intercalate "\nOR"
  <<< map condition
  where
    condition :: { entity_id :: Int | r } -> String
    condition = F.format ( SProxy :: SProxy " entity_id=\"{entity_id}\" ")


batchGetSalesFlatCreditMemoGrid
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f { entity_id :: Int | r }
  -> Aff (Either String (Array SalesFlatCreditMemoGrid))
batchGetSalesFlatCreditMemoGrid conn =
  (MySQL.query'_ <@> conn) <<< _batchGetSalesFlatCreditMemoGrid
