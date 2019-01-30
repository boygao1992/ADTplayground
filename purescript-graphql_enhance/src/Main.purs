module Main where

import Prelude
import Effect (Effect)

import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic, Constructor, Argument)
import GraphQL.Type.Internal (GraphQLType, Id)
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import GraphQL.Type.Internal.ToObjectTypeField (class FetchScalarFields, class ToFieldList, toObject)
import Examples.ForumExample.Model (Comment, Post)
import Prim.RowList as RowList
import Effect.Aff (Aff)
import Data.Nullable (Nullable)

newtype User = User
  { id :: Id
  , posts :: { date :: String } -> Array Post
  , comments :: { limit :: Int } -> Array Comment
  , post :: { id :: Id } -> Post
  }

derive instance genericUser :: Generic User _

toFieldType
  :: forall spec specName specRow specRl specFl source
   . Generic spec (Constructor specName (Argument (Record specRow)))
  => RowList.RowToList specRow specRl
  => ToFieldList specRl specFl
  => FetchScalarFields specFl source
  => Proxy spec
  -> RProxy source
toFieldType _ = RProxy :: RProxy source

toFieldTypeText :: RProxy
  ( id :: String
  )
toFieldTypeText = toFieldType (Proxy :: Proxy User)

-- TODO testing
toObjectTest :: { comments :: { source :: { id :: String
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
           -> Aff Id
          )
, post :: { source :: { id :: String
                      }
          , args :: { id :: String
                    }
          }
          -> Aff
               { id :: String
               }
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
-> { "Comment" :: Unit -> Nullable (GraphQLType Comment)
   , "Post" :: Unit -> Nullable (GraphQLType Post)
   }
   -> GraphQLType (Maybe User)
toObjectTest =
  toObject
    (Proxy :: Proxy User)
    -- { id: Nothing
    -- , posts: \({ args: { date } }) -> pure []
    -- , comments: \({ args: { limit } }) -> pure []
    -- }


main :: Effect Unit
main = do
  pure unit
