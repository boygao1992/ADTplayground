module Magento.Migration.Grid.SQL.BatchGetSalesFlatShipmentGrid
( batchGetSalesFlatShipmentGrid
) where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (SalesFlatShipmentGrid)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchGetSalesFlatShipmentGrid
  :: forall f r
  .  Foldable f
  => Functor f
  => f { entity_id :: Int | r }
  -> String
_batchGetSalesFlatShipmentGrid =
  ("""
SELECT
    entity_id
  , store_id
  , total_qty
  , order_id
  , shipment_status
  , increment_id
  , order_increment_id
  , created_at
  , order_created_at
  , shipping_name
FROM sales_flat_shipment_grid
WHERE""" <> _)
  <<< intercalate "\nOR"
  <<< map condition
  where
    condition :: { entity_id :: Int | r } -> String
    condition = F.format ( SProxy :: SProxy " entity_id=\"{entity_id}\" ")


batchGetSalesFlatShipmentGrid
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f { entity_id :: Int | r }
  -> Aff (Either String (Array SalesFlatShipmentGrid))
batchGetSalesFlatShipmentGrid conn =
  (MySQL.query'_ <@> conn) <<< _batchGetSalesFlatShipmentGrid
