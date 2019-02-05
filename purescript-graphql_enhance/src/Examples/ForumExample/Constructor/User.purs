module Examples.ForumExample.Constructor.User where

import Data.Array (range)
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
      { source :: { id :: String
                  }
      , args :: { limit :: Int
                }
      }
      -> Aff
          ( Array
              { id :: String
              }
          )
  , id ::
      Maybe
        ( { source :: { id :: String
                      }
          }
          -> Aff String
        )
  , posts ::
      { source :: { id :: String
                  }
      , args :: { date :: String
                }
      }
      -> Aff
          ( Array
              { id :: String
              }
          )
  }
  ->  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
      , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
      }
  -> GraphQLType (Maybe User)
userConstructor =
  toObject
  (Proxy :: Proxy User)

commentsResolver ::
  { source :: { id :: String
              }
  , args :: { limit :: Int
            }
  }
  -> Aff
      ( Array
        { id :: String
        }
      )
commentsResolver { source: { id }, args: { limit }} =
  pure $ (\n -> { id: id <> "_comments_" <> show n}) <$> range 1 limit

postsResolver ::
  { source :: { id :: String
              }
  , args :: { date :: String
            }
  }
  -> Aff
      ( Array
        { id :: String
        }
      )
postsResolver { source: { id }, args: { date }} =
  pure [ { id: id <> "_posts" } ]

user ::
  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
  , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
  }
  -> GraphQLType (Maybe User)
user =
  userConstructor
  { id: Nothing
  , posts: postsResolver
  , comments: commentsResolver
  }
