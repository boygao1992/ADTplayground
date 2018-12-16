module Schema.Query where

import Prelude
import Schema.Post (postType)
import Store (readPostById, readPosts)

import Context (Context)
import Data.Maybe (Maybe(..))
import GraphQL.Type as G

queryType :: G.ObjectType Context (Maybe Unit)
queryType =
  G.objectType
    "Query"
    (Just "The main query type")
    { post:
        G.field
          postType
          (Just "Fetch a post by its Id.")
          { id:
              G.argument
                (G.nonNull G.id)
                (Just "A unique identity for the post.")
          }
          \_ { id } ctx -> readPostById id ctx.store
    , posts:
        G.field'
          (G.nonNull $ G.list
            $ G.nonNull postType
          )
          (Just "Fetch all posts.")
          \_ ctx -> readPosts ctx.store
    }
