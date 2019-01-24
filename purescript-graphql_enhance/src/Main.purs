module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import GraphQL.Type.Internal (Id)
import GraphQL.Type.Internal.ToInputObject (toInputObjectWithPath)
import Type.Data.Symbol (SProxy(..))
import Type.Row (RProxy(..))

newtype User = User
  { id :: Id
  , name :: String
  }
derive instance genericUser :: Generic User _

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
main :: Effect Unit
main = do
  let test = toInputObjectWithPath
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
  pure unit
