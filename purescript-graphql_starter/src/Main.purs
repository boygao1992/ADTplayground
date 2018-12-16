module Main where

import Prelude

import Context (Context)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Console (error, log, logShow) as Console
import GraphQL (graphql)
import HTTPure as HTTPure
import Schema (schema)
import Store (createStore)

type GraphQLParams =
  { query :: String
  , variables :: Maybe Json
  , operationName :: Maybe String
  }

decodeParams :: Json -> Either String GraphQLParams
decodeParams json = do
  obj <- decodeJson json
  query <- obj .: "query"
  variables <- obj .:? "variables"
  operationName <- obj .:? "operationName"
  pure { query, variables, operationName }

createRouter :: Context -> HTTPure.Request -> HTTPure.ResponseM
createRouter ctx { body, method: HTTPure.Post, path : [ "graphql" ]} =
  case decodeParams =<< (jsonParser body) of
    Left error ->
      do
        HTTPure.badRequest error

    Right { query, variables, operationName } ->
      do
        (HTTPure.ok <<< stringify)
          =<< graphql schema query unit ctx variables operationName
createRouter _ { path } = do
  liftEffect $ Console.logShow path
  HTTPure.notFound

main :: Effect Unit
main = runAff_ (either (Console.error <<< show) pure) do
  store <- createStore
  let router = createRouter { store }
  liftEffect $ HTTPure.serve 8080 router $ Console.log "listening at localhost:8080"
