module Examples.ForumExample.Constructor.User where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect.Aff (Aff)
import Examples.ForumExample.Model (User, Comment, Post)
import GraphQL.Type.Internal (GraphQLType)
import GraphQL.Type.Internal.ToObject (toObject)
import Prelude
import Type.Proxy (Proxy (..))

userConstructor ::
  { comments ::
      { source :: { id :: String}
      , args :: { limit :: Int}
      }
      -> Aff (Array { id :: String})
  , id :: Maybe
            ({ source :: { id :: String}}
            -> Aff String
            )
  , posts :: { source :: { id :: String}
            , args :: { date :: String}
            }
            -> Aff (Array { id :: String})
  }
  ->  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
      , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
      }
  -> GraphQLType (Maybe User)
userConstructor =
  toObject
  (Proxy :: Proxy User)

user ::
  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
  , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
  }
  -> GraphQLType (Maybe User)
user =
  userConstructor
  { id: Nothing
  , posts: \({ source: { id }, args: { date }}) -> pure [ { id: id <> "_posts" } ]
  , comments: \({source: {id}, args: { limit }}) -> pure [ { id: id <> "_comments" } ]
  }
