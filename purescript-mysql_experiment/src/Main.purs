module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Console (log, logShow)
import Effect.Exception (message)
import MySQL as SQL
import Record.Format (format) as F
import Run (Run, AFF, runBaseAff)
import Run.Reader (_reader, READER, runReaderAt)
import Type.Data.Symbol (SProxy(..))

_aff = SProxy :: SProxy "aff"

type Customer =
  { entity_id :: Int
  , entity_type_id :: Int
  , website_id :: Int
  , attribute_set_id :: Int
  , email :: String
  , group_id :: Int
  , increment_id :: String
  , store_id :: Int
  , created_at :: String
  , updated_at :: String
  , is_active :: Int
  , disable_auto_group_change :: Int
  }

type SQL = String

getCustomerByIdSQL
  :: Int
  -> SQL
getCustomerByIdSQL id =
  F.format
  ( SProxy :: SProxy """
      SELECT *
      FROM customer_entity
      WHERE entity_id = {id}
    """
  )
  { id }

getCustomerById
  :: forall eff
   . Int
  -> Run
       (reader :: READER SQL.Connection, aff :: AFF | eff)
       (Array Customer)
getCustomerById id = SQL.query_ (getCustomerByIdSQL id)


connectionInfo :: SQL.ConnectionInfo
connectionInfo =
  SQL.defaultConnectionInfo
  { host = "127.0.0.1"
  , user = "root"
  , password = "root"
  , database = "oldpure"
  , timezone = "local"
  , port = 3300
  }

main :: Effect Unit
main = do
  pool <- SQL.createPool connectionInfo SQL.defaultPoolInfo
  Aff.runAff_ (callback pool) do
    (SQL.withPool <@> pool) \conn ->
      runBaseAff $ runReaderAt _reader conn do
        getCustomerById 2

  where
    callback pool (Left err) = do
      log ">>>>>>>>>>>>>>>>>>>"
      log $ message err
      SQL.closePool pool
    callback pool (Right customers) = do
      logShow customers
      SQL.closePool pool

