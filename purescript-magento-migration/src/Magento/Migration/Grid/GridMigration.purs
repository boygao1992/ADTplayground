module Magento.Migration.Grid.GridMigration where

import Prelude

import Control.Parallel (parTraverse_)
import Data.Either (Either, either)
import Data.Foldable (class Foldable)
import Data.List as List
import Data.List.Utils (batching)
import Effect.Aff (Aff)
import Magento.Migration.Grid.SQL (batchGetSalesFlatCreditMemoGrid, batchGetSalesFlatInvoiceGrid, batchGetSalesFlatOrderGrid, batchGetSalesFlatShipmentGrid, batchInsertSalesFlatCreditMemoGrid, batchInsertSalesFlatInvoiceGrid, batchInsertSalesFlatOrderGrid, batchInsertSalesFlatShipmentGrid, getSalesFlatCreditMemoGridEntityIdsToInsert, getSalesFlatInvoiceGridEntityIdsToInsert, getSalesFlatOrderGridEntityIdsToInsert, getSalesFlatShipmentGridEntityIdsToInsert)
import MySQL as MySQL

migrateData
  :: forall entityInsert
  .  { getEntityIds
        :: MySQL.Connection
        -> Aff (Either String (Array { entity_id :: Int }))
     , batchGet
        :: forall f r
        .  Foldable f
        => Functor f
        => MySQL.Connection
        -> f { entity_id :: Int | r }
        -> Aff (Either String (Array entityInsert))
     , batchInsert
        :: forall f
        .  Foldable f
        => Functor f
        => MySQL.Connection
        -> f entityInsert
        -> Aff Unit
     }
  -> { sourcePool :: MySQL.Pool
     , targetPool :: MySQL.Pool
     , batchSize :: Int
     }
  -> Aff Unit
migrateData
  { getEntityIds
  , batchGet
  , batchInsert
  }
  { sourcePool
  , targetPool
  , batchSize
  } = do
    entityIdsToInsert
      <- (MySQL.withPool <@> sourcePool) \conn ->
          either (const List.Nil) List.fromFoldable
          <$> getEntityIds conn

    (parTraverse_ <@> (batching batchSize entityIdsToInsert)) \ids -> do
      sourceEntities
        <- (MySQL.withPool <@> sourcePool) \conn ->
          either (const List.Nil) List.fromFoldable
          <$> batchGet conn ids

      (MySQL.withPool <@> targetPool) \conn ->
        batchInsert conn sourceEntities

migrateSalesFlatOrderGrid
  :: { sourcePool :: MySQL.Pool
     , targetPool :: MySQL.Pool
     , batchSize :: Int
     }
  -> Aff Unit
migrateSalesFlatOrderGrid =
  migrateData
  { getEntityIds: getSalesFlatOrderGridEntityIdsToInsert
  , batchGet: batchGetSalesFlatOrderGrid
  , batchInsert: batchInsertSalesFlatOrderGrid
  }

migrateSalesFlatShipmentGrid
  :: { sourcePool :: MySQL.Pool
     , targetPool :: MySQL.Pool
     , batchSize :: Int
     }
  -> Aff Unit
migrateSalesFlatShipmentGrid =
  migrateData
  { getEntityIds: getSalesFlatShipmentGridEntityIdsToInsert
  , batchGet: batchGetSalesFlatShipmentGrid
  , batchInsert: batchInsertSalesFlatShipmentGrid
  }

migrateSalesFlatCreditMemoGrid
  :: { sourcePool :: MySQL.Pool
     , targetPool :: MySQL.Pool
     , batchSize :: Int
     }
  -> Aff Unit
migrateSalesFlatCreditMemoGrid =
  migrateData
  { getEntityIds: getSalesFlatCreditMemoGridEntityIdsToInsert
  , batchGet: batchGetSalesFlatCreditMemoGrid
  , batchInsert: batchInsertSalesFlatCreditMemoGrid
  }

migrateSalesFlatInvoiceGrid
  :: { sourcePool :: MySQL.Pool
     , targetPool :: MySQL.Pool
     , batchSize :: Int
     }
  -> Aff Unit
migrateSalesFlatInvoiceGrid =
  migrateData
  { getEntityIds: getSalesFlatInvoiceGridEntityIdsToInsert
  , batchGet: batchGetSalesFlatInvoiceGrid
  , batchInsert: batchInsertSalesFlatInvoiceGrid
  }
