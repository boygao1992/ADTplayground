module Main where

-- TODO newtype StoreId, WebsiteId
-- TODO refactor logging into Run.Writer effect

import Prelude

import Control.Parallel (parSequence_)
import Control.Safely (for_)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Exception (message)
import Magento.Migration.Address.AddressMigrationDelta (addressMigrationDelta)
import Magento.Migration.Config (m1sourceConnectionInfo, m1sourcePoolInfo, m1targetConnectionInfo, m1targetPoolInfo, m2existingConnectionInfo, m2existingPoolInfo, m2existing_store_id, m2existing_website_id, m2existing_website_name, m2freshConnectionInfo, m2freshPoolInfo, m2fresh_website_id)
import Magento.Migration.Customer.CustomerMigrationDelta (customerMigrationDelta)
import Magento.Migration.Grid.GridMigration (migrateSalesFlatCreditMemoGrid, migrateSalesFlatInvoiceGrid, migrateSalesFlatOrderGrid, migrateSalesFlatShipmentGrid)
import MySQL as MySQL


cleanup :: Array MySQL.Pool -> Either Aff.Error Unit -> Effect Unit
cleanup pools e = do
  case e of
    Left err -> do
      log $ message err
    Right _ -> do
      pure unit
  for_ pools MySQL.closePool
  log $ "cleaned up"


customerAndAddressMigration :: Effect Unit
customerAndAddressMigration = do
  m2freshPool <- MySQL.createPool m2freshConnectionInfo m2freshPoolInfo
  m2existingPool <- MySQL.createPool m2existingConnectionInfo m2existingPoolInfo

  Aff.runAff_ (cleanup [m2freshPool, m2existingPool]) do
    customerMigrationDelta
      { sourcePool: m2freshPool
      , targetPool: m2existingPool
      , source_website_id: m2fresh_website_id
      , target_website_id: m2existing_website_id
      , target_store_id: m2existing_store_id
      , target_website_name: m2existing_website_name
      , insert_batch_size: 3000
      , update_batch_size: 3000
      }

    addressMigrationDelta
      { sourcePool: m2freshPool
      , targetPool: m2existingPool
      , insert_batch_size: 3000
      , update_batch_size: 3000
      }

gridMigration :: Effect Unit
gridMigration = do
  m1sourcePool <- MySQL.createPool m1sourceConnectionInfo m1sourcePoolInfo
  m1targetPool <- MySQL.createPool m1targetConnectionInfo m1targetPoolInfo
  Aff.runAff_ (cleanup [m1sourcePool, m1targetPool]) do
    parSequence_
      [ migrateSalesFlatOrderGrid
          { sourcePool: m1sourcePool
          , targetPool: m1targetPool
          , batchSize: 3000
          }
      , migrateSalesFlatShipmentGrid
          { sourcePool: m1sourcePool
          , targetPool: m1targetPool
          , batchSize: 3000
          }
      , migrateSalesFlatCreditMemoGrid
          { sourcePool: m1sourcePool
          , targetPool: m1targetPool
          , batchSize: 3000
          }
      , migrateSalesFlatInvoiceGrid
          { sourcePool: m1sourcePool
          , targetPool: m1targetPool
          , batchSize: 3000
          }
      ]

main :: Effect Unit
main = customerAndAddressMigration
