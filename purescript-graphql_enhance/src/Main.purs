module Main where

import Prelude

import Effect (Effect)
import GraphQL.Type.Internal (Id)
import GraphQL.Type.Internal.ToInputObject (toInputObject)
import Type.Row (RProxy(..))
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)

newtype User = User
  { id :: Id
  , name :: String
  }
derive instance newtypeUser :: Newtype User _
derive instance genericUser :: Generic User _

{- InputObject spec

( id :: Id
, str :: String
, num :: Number
, bool :: Boolean
, int :: Int
, listInt :: Array Int
, author :: User
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
         { name: '_content',
           fields:
            { todoList:
               { type:
                  { nonNull:
                     { GraphQLList:
                        { GraphQLInputObjectType:
                           { name: '_content_todoList-Item',
                             fields:
                              { todo: { type: { nonNull: 'GraphQLString' } },
                                id: { type: { nonNull: 'GraphQLID' } } } } } } } },
              date: { type: { nonNull: 'GraphQLString' } } } } } },
  comment:
   { type:
      { GraphQLInputObjectType:
         { name: '_comment',
           fields:
            { id: { type: { nonNull: 'GraphQLID' } },
              content: { type: { nonNull: 'GraphQLString' } } } } } },
  bool: { type: { nonNull: 'GraphQLBoolean' } },
  author:
   { type:
      { GraphQLInputObjectType:
         { name: '_author-User',
           fields:
            { name: { type: { nonNull: 'GraphQLString' } },
              id: { type: { nonNull: 'GraphQLID' } } } } } } }

-}
main :: Effect Unit
main = do
  let test = toInputObject (RProxy :: RProxy
                                      ( id :: Id
                                      , str :: String
                                      , num :: Number
                                      , bool :: Boolean
                                      , int :: Int
                                      , listInt :: Array Int
                                      , author :: User
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
