module Magento.Migration.Deprecated.Customer.BridgeAddressEntityId where

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
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect) as Effect
import Effect.Console (log, logShow)
import Effect.Exception (message)
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

type AddressCore =
  { entity_id :: Int
  , email :: String
  , street :: String
  }

type AddressXMLSchema =
  { "Items" ::
       { item ::
            Array
            { _entity_id :: Int
            , _email :: String
            , street :: String
            }
       }
  }

_batchGetAddressEntityId
  :: forall f r
     .  Foldable f
     => Functor f
     => Int
  -> f { email :: String | r }
  -> String
_batchGetAddressEntityId website_id =
      ( \str ->
          F.format
            ( SProxy :: SProxy """
SELECT c.email AS email, a.entity_id AS entity_id, a.street AS street
FROM customer_address_entity AS a
JOIN customer_entity AS c
WHERE a.parent_id = c.entity_id
  AND c.website_id = {website_id}
  AND
( """
            )
            { website_id }
          <> str
          <> "\n)")
  <<< intercalate "\nOR"
  <<< map _addressCondition
    where
      _addressCondition :: { email :: String | r } -> String
      _addressCondition =
        F.format ( SProxy :: SProxy " email=\"{email}\"" )


-- Normalization

addressNormalization
  :: { _entity_id :: Int
     , _email :: String
     , street :: String
     }
  -> AddressCore
addressNormalization { _entity_id, _email, street }
  = { entity_id: _entity_id
    , email: _email
    , street
    }


-- SQL
batchGetAddressEntityId
  :: forall eff f r
  .  Foldable f
  => Functor f
  => Int
  -> f { email :: String | r}
  -> Run
      (reader :: READER MySQL.Connection, aff :: AFF | eff)
      (Either String (Array AddressCore))
batchGetAddressEntityId store_id =
  SQL.query'_ <<< _batchGetAddressEntityId store_id

cleanup :: Array MySQL.Pool -> Either Aff.Error Unit -> Effect Unit
cleanup pools e = do
  case e of
    Left err -> do
      log $ message err
    Right _ -> do
      pure unit
  for_ pools MySQL.closePool
  log $ "cleaned up"

bridgeAddressEntityId
  :: MySQL.ConnectionInfo
  -> Int
  -> String
  -> Effect Unit
bridgeAddressEntityId
  targetConnectionInfo
  target_website_id
  filePath
  = do
  xml <- readTextFile UTF8 filePath
  case xml2json xml of
    Left err ->
      log err
    Right (obj :: AddressXMLSchema) -> do
      let (sourceAddresses :: List AddressCore)
              = map addressNormalization
            <<< List.fromFoldable
              $ obj."Items".item
      let sourceEntities
              = Map.fromFoldable
            <<< map (   (   String.toLower <<< _.email
                        &&& _.street
                        )
                    &&& SourceOnly <<< _.entity_id
                    )
              $ sourceAddresses

      targetPool <- MySQL.createPool targetConnectionInfo MySQL.defaultPoolInfo
      Aff.runAff_ (cleanup [targetPool]) <<< Run.runBaseAff $ do
        targetEntities <-
          getAddresses targetPool sourceAddresses 100 TargetOnly target_website_id

        let bridged = Map.unionWith (<>) sourceEntities targetEntities
        Run.liftAff $ Effect.liftEffect $ logShow $ List.filter (not <<< is _bridged) $ List.fromFoldable bridged

    where
      getAddresses
        :: forall eff r
        .  MySQL.Pool
        -> List { email :: String | r }
        -> Int
        -> (Int -> Bridging Int Int)
        -> Int
        -> Run
            (aff :: AFF | eff)
            (Map (Tuple String String) (Bridging Int Int))
      getAddresses pool addresses batchSize bridgingConstructor website_id =
        SQL.withPool pool
        $ Map.fromFoldable
        <<< map (   (   String.toLower <<< _.email
                    &&& _.street
                    )
                &&& bridgingConstructor <<< _.entity_id
                )
        <<< List.fromFoldable
        <$> foldM
              (\acc batch -> do
                  o <-
                    Seq.fromFoldable
                    <<< either (const []) identity
                    <$> batchGetAddressEntityId website_id batch
                  pure $ acc <> o
              )
              Seq.empty
              (batching batchSize addresses)
