module Generic.AddDescription where

import Prelude

{-
  newtype Post = Post -- ObjectType
    { id :: Id
    , title :: String
    , content :: String
    }

  argument
    { fields : { title :: String } -- any subset of fields
    , this : String
    }

  output
    { fields : { id :: Maybe String
               , title :: Maybe String
               , content :: Maybe String
               }
    , this : Maybe String
    }

  G.objectType
    "Post"
    (Just "A persisted post.") <- this
    { id:
        G.field'
          (G.nonNull G.id)
          (Just "A unique identity for the post.") <- fields.id
          (\post _ -> pure post.title)
    , title:
        G.field'
          (G.nonNull G.string)
          (Just "The title of the post.") <- fields.title
          (\post _ -> pure post.title)
    , content:
        G.field'
          (G.nonNull G.string)
          (Just "The content of the post.") <- fields.content
          (\post _ -> pure post.content)
    }

  genericGraphQLObjectType
    :: forall a ctx rep des res
     . Generic a rep
    => GenericAddDescription rep des
    => GenericAddResolvers rep res
    => Proxy a
    -> des
    -> res
    -> G.ObjectType ctx a

  foreign import kind GraphQLType
  foreign import data ObjectType :: GraphQLType
  foreign import data InputObjectType :: GraphQLType

  class GraphQLSubType ps (gl :: GraphQLType)

  used to dispatch to one of the generic constructors
  - genericGraphQLObjectType
  - genericGraphQLInputObjectType

  e.g.
  instance graphQLSubTypePost :: GraphQLSubType Post ObjectType
  instance grqphQLSubTypePostAction :: GraphQLSubType PostAction InputObjectType
-}

