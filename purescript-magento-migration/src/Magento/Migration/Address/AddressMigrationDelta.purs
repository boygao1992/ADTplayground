module Magento.Migration.Address.AddressMigrationDelta where

import Prelude

import Control.Parallel (parTraverse_)
import Control.Safely (foldM)
import Data.Array as Array
import Data.Either (either)
import Data.Formatter.DateTime (format) as DateTime
import Data.List ((:))
import Data.List as List
import Data.List.Utils (batching)
import Data.Map as Map
import Data.Map.Bridging (bridging)
import Data.Profunctor.Strong ((&&&))
import Data.UUID (genUUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as Effect
import Effect.Console (log)
import Effect.Now (nowDateTime) as Now
import Magento.Migration.Address.SQL (batchGetAddressCore, batchGetAddresses, batchInsertAddresses, batchUpdateAddressMappingTargetInsert, batchUpdateAddressMappingTargetUpdate, batchUpdateAddresses, deltaGetAddressEntityIdToInsert, deltaGetAddressEntityIdToUpdate, fullInsertAddressMappingSource)
import MySQL as MySQL
import MySQL.Formatter (timeStampFormatter)
import Record as Record
import Type.Data.Symbol (SProxy(..))

addressMigrationDelta
  :: { sourcePool :: MySQL.Pool
     , targetPool :: MySQL.Pool
     , insert_batch_size :: Int
     , update_batch_size :: Int
     }
  -> Aff Unit
addressMigrationDelta
  { sourcePool
  , targetPool
  , insert_batch_size
  , update_batch_size
  }
  = do
    startTime <- DateTime.format timeStampFormatter
                  <$> Effect.liftEffect Now.nowDateTime
    (MySQL.withPool <@> sourcePool)
      $ fullInsertAddressMappingSource <@> startTime

    entityIdsToInsert
      <- (MySQL.withPool <@> sourcePool)
        $ \conn ->
          List.fromFoldable
          <<< either (const []) identity
          <$> (deltaGetAddressEntityIdToInsert conn)

    entityIdPairsToUpdate
      <- (MySQL.withPool <@> sourcePool)
        $ \conn ->
          List.fromFoldable
          <<< either (const []) identity
          <$> (deltaGetAddressEntityIdToUpdate conn)

    parTraverse_
      (\ids -> do
          Effect.liftEffect $ log $ show (List.head ids) <> " Insert start"

          sourceAddresses <- (MySQL.withPool <@> sourcePool) \conn ->
            either (const List.Nil) List.fromFoldable
              <$> batchGetAddresses conn ids

          targetAddresses <- foldM
            (\acc x -> do
                uuid <- show <$> Effect.liftEffect genUUID
                pure $ (Record.insert (SProxy :: SProxy "uuid") uuid x) : acc
            )
            List.Nil
            sourceAddresses

          targetCores <- (MySQL.withPool <@> targetPool) \conn -> do
            batchInsertAddresses conn targetAddresses
            either (const []) identity
              <$> batchGetAddressCore conn targetAddresses

          currentTime <- DateTime.format timeStampFormatter
                        <$> Effect.liftEffect Now.nowDateTime

          let bridged =
                (\({ left, right }) ->
                  { source_address_id: left.entity_id
                  , target_address_id: right.entity_id
                  , uuid: right.uuid
                  , synced_at: currentTime
                  , last_update: left.updated_at
                  })
                <$> bridging
                      targetAddresses
                      _.uuid
                      targetCores
                      _.uuid

          (MySQL.withPool <@> sourcePool) \conn ->
            batchUpdateAddressMappingTargetInsert conn bridged

          Effect.liftEffect $ log $ show (List.head ids) <> " Insert end"
      )
      (batching insert_batch_size entityIdsToInsert)

    parTraverse_
      (\idPairs -> do
          Effect.liftEffect $ log $ show (List.head idPairs) <> " Insert start"

          sourceAddresses <- (MySQL.withPool <@> sourcePool)
            $ \conn ->
              map (either (const []) identity)
              <<< batchGetAddresses conn
              <<< map (\c -> {entity_id: c.source_address_id})
                $ idPairs

          let mapping =
                Map.fromFoldable
                <<< map (_.source_address_id &&& _.target_address_id)
                  $ idPairs
          let targetAddresses =
                Array.mapMaybe
                  (\c ->
                    (\entity_id -> c { entity_id = entity_id })
                    <$> Map.lookup c.entity_id mapping
                  )
                  $ sourceAddresses
          (MySQL.withPool <@> targetPool) \conn -> do
            batchUpdateAddresses conn targetAddresses

          currentTime <- DateTime.format timeStampFormatter
                          <$> Effect.liftEffect Now.nowDateTime

          let bridged =
                (\({ left, right }) ->
                  { source_address_id: right.source_address_id
                  , target_address_id: right.target_address_id
                  , synced_at: currentTime
                  , last_update: left.updated_at
                  })
                <$> bridging
                      sourceAddresses
                      _.entity_id
                      idPairs
                      _.source_address_id

          (MySQL.withPool <@> sourcePool) \conn ->
            batchUpdateAddressMappingTargetUpdate conn bridged

          Effect.liftEffect $ log $ show (List.head idPairs) <> " Insert end"
      )
      (batching update_batch_size entityIdPairsToUpdate)
