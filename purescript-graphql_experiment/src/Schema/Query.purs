module Schema.Query where

import Prelude

import Schema.Message
import Context (Context)
import Data.Maybe (Maybe(..))
import GraphQL.Type as G

queryType :: G.ObjectType Context (Maybe Unit)
queryType =
  G.objectType
  "Query"
  (Just "Query root node")
  { message:
      G.field'
        (G.nonNull messageType)
        Nothing
        \_ ctx -> pure $ { id: "", content: Just "message from parent" }
  }
