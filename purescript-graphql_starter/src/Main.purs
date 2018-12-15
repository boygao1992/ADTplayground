module Main where

import Prelude
import Data.Maybe(Maybe(..))
import Data.Either(either)
import Effect (Effect)
import Effect.Console (log, error) as Console
import Data.Argonaut.Core (stringify) as JSON

import Effect.Aff (runAff_)
import GraphQL (graphql)
import GraphQL.Type as GraphQL

queryType :: GraphQL.ObjectType Unit (Maybe Unit)
queryType =
  GraphQL.objectType
    "Query"
    (Just "The main query type") -- description
    { hello:
        GraphQL.field'
          (GraphQL.nonNull GraphQL.string)
          (Just "A simple field that always returns \"world\".")
          \_ _ -> pure "world"
    }
schema :: GraphQL.Schema Unit Unit
schema = GraphQL.schema queryType Nothing

main :: Effect Unit
main = do
  runAff_ (either (Console.error <<< show) (Console.log <<< JSON.stringify))
    $ graphql schema "{ hello }" unit unit Nothing Nothing
