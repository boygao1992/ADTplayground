module Magento.Migration.Deprecated.Customer.BridgeCustomerEntityId where

import Prelude

import Control.Safely (for_, foldM)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, intercalate)
import Data.Lens (is)
import Data.List (List)
import Data.List as List
import Data.List.Utils (batching)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Bridging (Bridging(..), _bridged)
import Data.Profunctor.Strong ((&&&))
import Data.Sequence as Seq
import Data.String as String
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect) as Effect
import Effect.Console (log, logShow)
import Effect.Exception (message)
import Magento.Migration.Types (CustomerCore)
import MySQL (Connection, ConnectionInfo, Pool, closePool, createPool, defaultPoolInfo) as MySQL
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Record.Format (format) as F
import Run (AFF, Run)
import Run as Run
import Run.MySQL as SQL
import Run.Reader (READER)
import Type.Data.Symbol (SProxy(..))
import XML (xml2json)

type CustomerXMLSchema =
  { "Items" ::
       { item ::
            Array
            { email :: String
            }
       }
  }

_batchGetCustomerCore
  :: forall f r
  .  Foldable f
  => Functor f
  => Int
  -> f { email :: String | r }
  -> String
_batchGetCustomerCore website_id =
      ( \str ->
          F.format
            ( SProxy :: SProxy """
SELECT entity_id, website_id, email
FROM customer_entity
WHERE website_id = {website_id} AND
( """
            )
            { website_id }
        <> str
        <> "\n)")
  <<< intercalate "\nOR"
  <<< map _customerCondition
    where
      _customerCondition :: { email :: String | r} -> String
      _customerCondition = F.format ( SProxy :: SProxy " email=\"{email}\" ")


batchGetCustomerCore
  :: forall eff f r
  .  Foldable f
  => Functor f
  => Int
  -> f { email :: String | r}
  -> Run
      (reader :: READER MySQL.Connection, aff :: AFF | eff)
      (Either String (Array CustomerCore))
batchGetCustomerCore website_id =
  SQL.query'_ <<< _batchGetCustomerCore website_id

cleanup :: Array MySQL.Pool -> Either Aff.Error Unit -> Effect Unit
cleanup pools e = do
  case e of
    Left err -> do
      log $ message err
    Right _ -> do
      pure unit
  for_ pools MySQL.closePool
  log $ "cleaned up"


bridgeCustomerEntityId
  :: MySQL.ConnectionInfo
  -> MySQL.ConnectionInfo
  -> Int
  -> Int
  -> String
  -> Effect Unit
bridgeCustomerEntityId
  sourceConnectionInfo
  targetConnectionInfo
  source_website_id
  target_website_id
  filePath
  = do
    xml <- readTextFile UTF8 filePath
    case xml2json xml of
      Left err ->
        log err
      Right (obj :: CustomerXMLSchema) -> do
        let customers = List.fromFoldable obj."Items".item

        sourcePool <- MySQL.createPool sourceConnectionInfo MySQL.defaultPoolInfo
        targetPool <- MySQL.createPool targetConnectionInfo MySQL.defaultPoolInfo

        Aff.runAff_ (cleanup [sourcePool, targetPool]) <<< Run.runBaseAff $ do
          sourceEntities <-
            getCustomers sourcePool customers 100 SourceOnly source_website_id
          targetEntities <-
            getCustomers targetPool customers 100 TargetOnly target_website_id

          let bridged = Map.unionWith (<>) sourceEntities targetEntities

          Run.liftAff $ Effect.liftEffect $ logShow $ List.filter (not <<< is _bridged) $ List.fromFoldable bridged

    where
      getCustomers
        :: forall eff r
        .  MySQL.Pool
        -> List { email :: String | r }
        -> Int
        -> (Int -> Bridging Int Int)
        -> Int
        -> Run
            (aff :: AFF | eff)
            (Map String (Bridging Int Int))
      getCustomers pool customers batchSize bridgingConstructor website_id =
        SQL.withPool pool
        $ Map.fromFoldable
        <<< map (   String.toLower <<< _.email
                &&& bridgingConstructor <<< _.entity_id
                )
        <<< List.fromFoldable
        <$> foldM
              (\acc batch -> do
                  o <-
                    Seq.fromFoldable
                    <<< either (const []) identity
                    <$> batchGetCustomerCore website_id batch
                  pure $ acc <> o
              )
              Seq.empty
              (batching batchSize customers)
