module Examples.ForumExample.Constructor.Post where

import Prelude

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect.Aff (Aff)
import Examples.ForumExample.Model (User, Comment, Post)
import GraphQL.Type.Internal (GraphQLType)
import GraphQL.Type.Internal.ToObject (toObject)
import Type.Proxy (Proxy(..))

postConstructor ::
  { author ::
      { source :: { id :: String
                  }
      }
      -> Aff
            { id :: String
            }
  , comments ::
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
  }
  ->  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
      , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
      }
  -> GraphQLType (Maybe Post)
postConstructor =
  toObject (Proxy :: Proxy Post)

authorResolver ::
  { source :: { id :: String
              }
  }
  -> Aff
      { id :: String
      }
authorResolver { source: { id }} = pure { id: id <> "_author" }

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

post ::
  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
  , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
  }
  -> GraphQLType (Maybe Post)
post =
  postConstructor
  { id: Nothing
  , author: authorResolver
  , comments: commentsResolver
  }
