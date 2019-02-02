module Test.Main where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Console (error, log, logShow) as Console
import GraphQL.Type.Internal (graphql)
import HTTPure as HTTPure
import Test.ToRootObject (testSchema)

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

createRouter :: HTTPure.Request -> HTTPure.ResponseM
createRouter { body, method: HTTPure.Post, path : [ "graphql" ]} =
  case decodeParams =<< (jsonParser body) of
    Left error ->
      do
        HTTPure.badRequest error

    Right { query, variables, operationName } ->
      do
        (HTTPure.ok <<< stringify)
          =<< graphql testSchema query unit unit variables operationName
createRouter { path } = do
  liftEffect $ Console.logShow path
  HTTPure.notFound

main :: Effect Unit
main = runAff_ (either (Console.error <<< show) pure) do
  liftEffect $ HTTPure.serve 8080 createRouter $ Console.log "listening at localhost:8080"
