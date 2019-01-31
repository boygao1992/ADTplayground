module Test.CollectEntities where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Examples.ForumExample.Model (Comment, Post, User)
import GraphQL.Type.Internal (GraphQLType)
import GraphQL.Type.Internal.ToRootObject (class CollectConstructors, class CollectEntities)
import Type.Data.List (Cons, LProxy(..), Nil)
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))

collect :: forall spec entities. CollectEntities spec entities => Proxy spec -> LProxy entities
collect _ = LProxy :: LProxy entities

collectTest :: LProxy (Cons Comment (Cons Post (Cons User Nil)))
collectTest =
  collect (Proxy :: Proxy User)

  -- Test

collectConstructors :: forall entities constructors. CollectConstructors entities constructors => LProxy entities -> RProxy constructors
collectConstructors _ = RProxy :: RProxy constructors

collectConstructorsExample :: RProxy
  ( "Comment" :: { "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
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
  )
collectConstructorsExample =
  collectConstructors
    collectTest
