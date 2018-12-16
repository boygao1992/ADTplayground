module Main where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Core (stringify) as JSON
import Data.Argonaut.Decode (class DecodeJson, decodeJson, getField, getFieldOptional')
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Either (either)
import Data.Generic.Rep as GR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log, error) as Console
import GraphQL (graphql)
import GraphQL.Type as G

newtype GraphQLParams = GraphQLParams
  { query :: String
  , variables :: Maybe Json
  , operationName :: Maybe String
  }
derive instance genericGraphQLParams :: GR.Generic GraphQLParams _
instance decodeJsonGraphQLParams :: DecodeJson GraphQLParams where
  decodeJson = genericDecodeJson

queryType :: G.ObjectType Unit (Maybe Unit)
queryType =
  G.objectType
    "Query"
    (Just "The main query type") -- description
    { hello:
        G.field'
          (G.nonNull G.string)
          (Just "A simple field that always returns \"world\".")
          \_ _ -> pure "world"
    }
schema :: G.Schema Unit Unit
schema = G.schema queryType Nothing

main :: Effect Unit
main = do
  runAff_ (either (Console.error <<< show) (Console.log <<< JSON.stringify))
    $ graphql schema "{ hello }" unit unit Nothing Nothing
