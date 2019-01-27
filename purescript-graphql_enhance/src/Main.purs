module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import GraphQL.Type.Internal (GraphQLType, Id)
import GraphQL.Type.Internal.ToInputObject (toInputObjectWithPath)
import Type.Data.Symbol (SProxy(..))
import Type.Row (RProxy(..))

newtype User = User
  { id :: Id
  , name :: String
  , background :: UserBackground
  }
derive instance genericUser :: Generic User _

newtype UserBackground = UserBackground
  { age :: Int
  }
derive instance genericUserBackground :: Generic UserBackground _

{- InputObject spec

( id :: Id
, str :: String
, num :: Number
, bool :: Boolean
, int :: Int
, listInt :: Array Int
, author :: User
, authors :: Array User
, comment ::  { id :: Id
              , content :: String
              }
, content ::
    { date :: String
    , todoList :: Array
                    { id :: Id
                    , todo :: String
                    }
    }
)

-}
{- NOTE inspect for testing

{ str: { type: { nonNull: 'GraphQLString' } },
  num: { type: { nonNull: 'GraphQLFloat' } },
  listInt:
   { type: { nonNull: { GraphQLList: { nonNull: 'GraphQLInt' } } } },
  int: { type: { nonNull: 'GraphQLInt' } },
  id: { type: { nonNull: 'GraphQLID' } },
  content:
   { type:
      { GraphQLInputObjectType:
         { name: 'Post_createPost_content',
           fields:
            { todoList:
               { type:
                  { nonNull:
                     { GraphQLList:
                        { GraphQLInputObjectType:
                           { name: 'Post_createPost_content_todoList-Item',
                             fields:
                              { todo: { type: { nonNull: 'GraphQLString' } },
                                id: { type: { nonNull: 'GraphQLID' } } } } } } } },
              date: { type: { nonNull: 'GraphQLString' } } } } } },
  comment:
   { type:
      { GraphQLInputObjectType:
         { name: 'Post_createPost_comment',
           fields:
            { id: { type: { nonNull: 'GraphQLID' } },
              content: { type: { nonNull: 'GraphQLString' } } } } } },
  bool: { type: { nonNull: 'GraphQLBoolean' } },
  authors:
   { type:
      { nonNull:
         { GraphQLList:
            { GraphQLInputObjectType:
               { name: 'Post_createPost_authors-Item-User',
                 fields:
                  { name: { type: { nonNull: 'GraphQLString' } },
                    id: { type: { nonNull: 'GraphQLID' } } } } } } } },
  author:
   { type:
      { GraphQLInputObjectType:
         { name: 'Post_createPost_author-User',
           fields:
            { name: { type: { nonNull: 'GraphQLString' } },
              id: { type: { nonNull: 'GraphQLID' } } } } } } }
-}


test :: { author :: { "type" :: GraphQLType User
            }
, authors :: { "type" :: GraphQLType (Array User)
             }
, bool :: { "type" :: GraphQLType Boolean
          }
, comment :: { "type" :: GraphQLType
                           { id :: Id
                           , content :: String
                           }
             }
, content :: { "type" :: GraphQLType
                           { date :: String
                           , todoList :: Array
                                           { id :: Id
                                           , todo :: String
                                           }
                           }
             }
, id :: { "type" :: GraphQLType Id
        }
, int :: { "type" :: GraphQLType Int
         }
, listInt :: { "type" :: GraphQLType (Array Int)
             }
, num :: { "type" :: GraphQLType Number
         }
, str :: { "type" :: GraphQLType String
         }
}
test = toInputObjectWithPath
        (SProxy :: SProxy "Post_createPost")
        (RProxy :: RProxy
                ( id :: Id
                , str :: String
                , num :: Number
                , bool :: Boolean
                , int :: Int
                , listInt :: Array Int
                , author :: User
                , authors :: Array User
                , comment ::  { id :: Id
                              , content :: String
                              }
                , content ::
                    { date :: String
                    , todoList :: Array
                                    { id :: Id
                                    , todo :: String
                                    }
                    }
                )
      )

main :: Effect Unit
main = do
  pure unit
