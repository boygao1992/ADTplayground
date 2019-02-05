module Examples.ForumExample.Query where

import Examples.ForumExample.Constructor (comment, post, user)
import Prelude

import Data.Array (range)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Examples.ForumExample.Model (User, Comment, Post)
import GraphQL.Type.Internal (GraphQLRootType, Id)
import GraphQL.Type.Internal.ToRootObject (toRootObject)
import Type.Proxy (Proxy(..))


newtype Query = Query
  { user :: { id :: Id } -> Maybe User
  , post :: { id :: Id } -> Maybe Post
  , comment :: { id :: Id } -> Maybe Comment
  , posts :: { limit :: Int } -> Array Post
  , comments :: { limit :: Int } -> Array Comment
  }

derive instance genericQuery :: Generic Query _

queryConstructor :: { comment :: { source :: Unit
             , args :: { id :: String
                       }
             }
             -> Aff
                  (Maybe
                     { id :: String
                     }
                  )
, comments :: { source :: Unit
              , args :: { limit :: Int
                        }
              }
              -> Aff
                   (Array
                      { id :: String
                      }
                   )
, post :: { source :: Unit
          , args :: { id :: String
                    }
          }
          -> Aff
               (Maybe
                  { id :: String
                  }
               )
, posts :: { source :: Unit
           , args :: { limit :: Int
                     }
           }
           -> Aff
                (Array
                   { id :: String
                   }
                )
, user :: { source :: Unit
          , args :: { id :: String
                    }
          }
          -> Aff
               (Maybe
                  { id :: String
                  }
               )
}
-> GraphQLRootType Query Unit
queryConstructor =
  toRootObject
    (Proxy :: Proxy Query)
    { "User": user
    , "Post": post
    , "Comment": comment
    }
    (Proxy :: Proxy Unit)

userResolver :: forall source.
  { source :: source
  , args :: { id :: String
            }
  }
  -> Aff (Maybe { id :: String})
userResolver { args: { id } } =
  pure $ pure { id: id <> "_user" }

postResolver :: forall source.
  { source :: source
  , args :: { id :: String}
  }
  -> Aff (Maybe { id :: String})
postResolver { args: { id }} =
  pure $ pure { id: id <> "_post"}

commentResolver :: forall source.
  { source :: source
  , args :: { id :: String
            }
  }
  -> Aff (Maybe { id :: String})
commentResolver { args: { id }} =
  pure $ pure { id: id <> "_comment"}

postsResolver :: forall source.
  { source :: source
  , args :: { limit :: Int}
  }
  -> Aff (Array { id :: String})
postsResolver { args: { limit }} =
  pure $ (\n -> { id: show n <> "_posts" }) <$> range 1 limit

commentsResolver :: forall source.
  { source :: source
  , args :: { limit :: Int
            }
  }
  -> Aff (Array { id :: String})
commentsResolver { args: { limit }} =
  pure $ (\n -> { id: show n <> "_comments"}) <$> range 1 limit

query :: GraphQLRootType Query Unit
query =
  queryConstructor
  { user: userResolver
  , post: postResolver
  , comment: commentResolver
  , posts: postsResolver
  , comments: commentsResolver
  }
