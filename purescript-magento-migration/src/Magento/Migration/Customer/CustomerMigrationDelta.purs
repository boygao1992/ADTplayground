module Magento.Migration.Customer.CustomerMigrationDelta
( customerMigrationDelta
)where

import Prelude

import Control.Parallel (parTraverse_)
import Data.Array as Array
import Data.Either (either)
import Data.Formatter.DateTime (format) as DateTime
import Data.List as List
import Data.List.Utils (batching)
import Data.Map as Map
import Data.Map.Bridging (bridging)
import Data.Nullable (notNull)
import Data.Profunctor.Strong ((&&&))
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as Effect
import Effect.Console (log)
import Effect.Now (nowDateTime) as Now
import Magento.Migration.Customer.SQL (batchGetCustomerCore, batchGetCustomers, batchInsertCustomers, batchUpdateCustomerMappingTarget, batchUpdateCustomers, deltaGetCustomerEntityIdToInsert, deltaGetCustomerEntityIdToUpdate, fullInsertCustomerMappingSource)
import MySQL as MySQL
import MySQL.Formatter (timeStampFormatter)

customerMigrationDelta
  :: { sourcePool :: MySQL.Pool
     , targetPool :: MySQL.Pool
     , source_website_id :: Int
     , target_website_id :: Int
     , target_store_id :: Int
     , target_website_name :: String
     , insert_batch_size :: Int
     , update_batch_size :: Int
     }
  -> Aff Unit
customerMigrationDelta
  { sourcePool
  , targetPool
  , source_website_id
  , target_website_id
  , target_store_id
  , target_website_name
  , insert_batch_size
  , update_batch_size
  }
  = do
  startTime <- DateTime.format timeStampFormatter
                  <$> Effect.liftEffect Now.nowDateTime

  (MySQL.withPool <@> sourcePool) $ \conn ->
    fullInsertCustomerMappingSource
      conn
      { source_website_id
      , created_at: startTime
      }

  entityIdsToInsert
    <- (MySQL.withPool <@> sourcePool)
      $ \conn ->
        List.fromFoldable
        <<< either (const []) identity
        <$> (deltaGetCustomerEntityIdToInsert conn source_website_id)

  entityIdPairsToUpdate
    <- (MySQL.withPool <@> sourcePool)
      $ \conn ->
        List.fromFoldable
        <<< either (const []) identity
        <$> (deltaGetCustomerEntityIdToUpdate conn source_website_id)

  -- Insert
  parTraverse_
    (\ids -> do
        Effect.liftEffect $ log $ show (List.head ids) <> " Insert start"

        sourceCustomers <- (MySQL.withPool <@> sourcePool) \conn ->
          either (const []) identity
            <$> batchGetCustomers conn source_website_id ids

        let targetCustomers =
              (_ { website_id = target_website_id
                  , store_id = target_store_id
                  , created_in = notNull target_website_name
                  }
              ) <$> sourceCustomers
        targetCores <- (MySQL.withPool <@> targetPool) \conn -> do
          batchInsertCustomers conn targetCustomers
          either (const []) identity
            <$> batchGetCustomerCore conn target_website_id targetCustomers

        currentTime <- DateTime.format timeStampFormatter
                        <$> Effect.liftEffect Now.nowDateTime

        let bridged =
              (\({ left, right }) ->
                { source_customer_id: left.entity_id
                , target_customer_id: right.entity_id
                , synced_at: currentTime
                , last_update: left.updated_at
                })
              <$> bridging
                    sourceCustomers
                    _.email
                    targetCores
                    _.email

        (MySQL.withPool <@> sourcePool) \conn ->
          batchUpdateCustomerMappingTarget conn bridged

        Effect.liftEffect $ log $ show (List.head ids) <> " Insert end"
    )
    (batching insert_batch_size entityIdsToInsert)

  -- Update
  parTraverse_
    (\idPairs -> do
        Effect.liftEffect $ log $ show (List.head idPairs) <> " Update start"

        sourceCustomers <- (MySQL.withPool <@> sourcePool) \conn ->
          map (either (const []) identity)
          <<< batchGetCustomers conn source_website_id
          <<< map (\c -> {entity_id: c.source_customer_id})
            $ idPairs

        let mapping =
              Map.fromFoldable
              <<< map (_.source_customer_id &&& _.target_customer_id)
                $ idPairs
        let targetCustomers =
              map (_ { website_id = target_website_id
                      , store_id = target_store_id
                      , created_in = notNull target_website_name
                      }
                  )
              <<< Array.mapMaybe
                    (\c ->
                      (\entity_id -> c { entity_id = entity_id })
                        <$> Map.lookup c.entity_id mapping
                    )
                $ sourceCustomers
        (MySQL.withPool <@> targetPool) \conn ->
          batchUpdateCustomers conn targetCustomers

        currentTime <- DateTime.format timeStampFormatter
                        <$> Effect.liftEffect Now.nowDateTime
        let bridged =
              (\({ left, right }) ->
                { source_customer_id: right.source_customer_id
                , target_customer_id: right.target_customer_id
                , synced_at: currentTime
                , last_update: left.updated_at
                })
              <$> bridging
                    sourceCustomers
                    _.entity_id
                    idPairs
                    _.source_customer_id

        (MySQL.withPool <@> sourcePool) \conn ->
          batchUpdateCustomerMappingTarget conn bridged

        Effect.liftEffect $ log $ show (List.head idPairs) <> " Update end"
    )
    (batching update_batch_size entityIdPairsToUpdate)
