module Examples.ForumExample.Constructor.Comment where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect.Aff (Aff)
import Examples.ForumExample.Model (User, Comment, Post)
import GraphQL.Type.Internal (GraphQLType)
import GraphQL.Type.Internal.ToObject (toObject)
import Type.Proxy (Proxy(..))

commentConstructor :: { author :: { source :: { id :: String
                        }
            }
            -> Aff
                 { id :: String
                 }
, id :: Maybe
          ({ source :: { id :: String
                       }
           }
           -> Aff String
          )
, post :: { source :: { id :: String
                      }
          }
          -> Aff
               { id :: String
               }
}
-> { "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
   , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
   }
   -> GraphQLType (Maybe Comment)
commentConstructor =
  toObject (Proxy :: Proxy Comment)

comment ::
  { "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
  , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
  }
  -> GraphQLType (Maybe Comment)
comment =
  commentConstructor
  { id: Nothing
  , post: \({ source: {id}}) -> pure { id: id <> "_post" }
  , author: \({ source: {id}}) -> pure { id: id <> "_author" }
  }
