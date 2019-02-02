module Test.ToRootObject where

import Prelude

import Control.Monad.ST (ST)
import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Aff (Aff)
import Examples.ForumExample.Model (Comment, Post, User)
import GraphQL.Type.Internal (GraphQLRootType, GraphQLType, Id, Schema, schema)
import GraphQL.Type.Internal.ToObject (class ToFieldList, toObject)
import GraphQL.Type.Internal.ToRootObject (class CollectEntitiesTraverse, class FetchRelationalFieldList, dependencyRecord, initObjectRecord, toRootObject)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record.ST (STRecord)
import Type.Data.List (LProxy(..), Cons, Nil)
import Type.Proxy (Proxy(..))

newtype Query = Query
  { user :: { id :: Id } -> Maybe User
  , post :: { id :: Id } -> Maybe Post
  , posts :: { limit :: Int } -> Array Post
  }

derive instance genericQuery :: Generic Query _

userConstructor :: { comments :: { source :: { id :: String
                          }
              , args :: { limit :: Int
                        }
              }
              -> Aff
                   (Array
                      { id :: String
                      }
                   )
, id :: Maybe
          ({ source :: { id :: String
                       }
           }
           -> Aff String
          )
, posts :: { source :: { id :: String
                       }
           , args :: { date :: String
                     }
           }
           -> Aff
                (Array
                   { id :: String
                   }
                )
}
-> { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
   , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
   }
   -> GraphQLType (Maybe User)
userConstructor =
  toObject
    (Proxy :: Proxy User)

userConstructor' ::
  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
  , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
  }
  -> GraphQLType (Maybe User)
userConstructor' =
  userConstructor
  { id: Nothing
  , posts: \({ source: { id }, args: { date }}) -> pure []
  , comments: \({source: {id}, args: { limit }}) -> pure []
  }

postConstructor :: { author :: { source :: { id :: String
                        }
            }
            -> Aff
                 { id :: String
                 }
, comments :: { source :: { id :: String
                          }
              , args :: { limit :: Int
                        }
              }
              -> Aff
                   (Array
                      { id :: String
                      }
                   )
, id :: Maybe
          ({ source :: { id :: String
                       }
           }
           -> Aff String
          )
}
-> { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
   , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
   }
   -> GraphQLType (Maybe Post)
postConstructor =
  toObject
  (Proxy :: Proxy Post)

postConstructor' ::
  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
  , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
  }
  -> GraphQLType (Maybe Post)
postConstructor' =
  postConstructor
  { id: Nothing
  , author: \({ source: { id }}) -> pure { id }
  , comments: \({ source: { id }, args: { limit }}) -> pure []
  }

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

commentConstructor' ::
  { "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
  , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
  }
  -> GraphQLType (Maybe Comment)
commentConstructor' =
  commentConstructor
  { id: Nothing
  , post: \({ source: {id}}) -> pure { id }
  , author: \({ source: {id}}) -> pure { id }
  }

queryConstructor :: forall t770.
  { "Comment" :: { "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
                 , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
                 }
                 -> GraphQLType (Maybe Comment)
  , "Post" :: { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
              , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
              }
              -> GraphQLType (Maybe Post)
  , "User" :: { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
              , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
              }
              -> GraphQLType (Maybe User)
  }
  -> Proxy t770
     -> { post :: { source :: t770
                  , args :: { id :: String
                            }
                  }
                  -> Aff
                       (Maybe
                          { id :: String
                          }
                       )
        , posts :: { source :: t770
                   , args :: { limit :: Int
                             }
                   }
                   -> Aff
                        (Array
                           { id :: String
                           }
                        )
        , user :: { source :: t770
                  , args :: { id :: String
                            }
                  }
                  -> Aff
                       (Maybe
                          { id :: String
                          }
                       )
        }
        -> GraphQLRootType Query t770
queryConstructor =
  toRootObject
  (Proxy :: Proxy Query)

queryConstructor' :: { post :: { source :: Unit
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
queryConstructor' =
  queryConstructor
  { "User": userConstructor'
  , "Post": postConstructor'
  , "Comment": commentConstructor'
  }
  (Proxy :: Proxy Unit)

query :: GraphQLRootType Query Unit
query =
  queryConstructor'
  { user: \({ args: {id}}) -> pure $ pure {id}
  , post: \({ args: {id}}) -> pure $ pure {id}
  , posts: \({ args: {limit}}) -> do
      liftEffect $ logShow limit
      pure [ {id: "007"} ]
  }

testSchema :: Schema Query Unit
testSchema = schema query

-- toEntityList
toEntityList
  :: forall rootSpec rootSpecName rootSpecRow rootSpecRl rootSpecFl rootRelationalSpecList entityList
   . Generic rootSpec (Constructor rootSpecName (Argument (Record rootSpecRow)))
  => RowList.RowToList rootSpecRow rootSpecRl
  => ToFieldList rootSpecRl rootSpecFl
  => FetchRelationalFieldList rootSpecFl rootRelationalSpecList
  => CollectEntitiesTraverse rootRelationalSpecList entityList
  => Proxy rootSpec
  -> LProxy entityList
toEntityList _ = LProxy :: LProxy entityList

toEntityListTest :: LProxy (Cons Comment (Cons Post (Cons User Nil)))
toEntityListTest =
  toEntityList
  (Proxy :: Proxy Query)

initObjectRecordTest :: forall h.
  ST h
    (STRecord h
       ( "Comment" :: Nullable (GraphQLType (Maybe Comment))
       , "Post" :: Nullable (GraphQLType (Maybe Post))
       , "User" :: Nullable (GraphQLType (Maybe User))
       )
    )
initObjectRecordTest =
  initObjectRecord
    toEntityListTest

dependencyRecordTest :: forall h.
  ST h
    { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
    , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
    , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
    }
dependencyRecordTest =
  dependencyRecord =<< initObjectRecordTest
