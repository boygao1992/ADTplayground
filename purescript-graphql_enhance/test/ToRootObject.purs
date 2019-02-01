module Test.ToRootObject where

import GraphQL.Type.Internal.ToObject
import GraphQL.Type.Internal.ToRootObject
import Prelude

import Control.Monad.ST (ST)
import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Effect.Aff (Aff)
import Examples.ForumExample.Model (Comment, Post, User)
import GraphQL.Type.Internal (GraphQLRootType, GraphQLType, Id)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record.ST (STRecord)
import Type.Data.List (LProxy(..), Cons, Nil)
import Type.Proxy (Proxy(..))

newtype Query = Query
  { user :: { id :: Id } -> User
  }

derive instance genericQuery :: Generic Query _

toRootObjectTypeTest ::
  { "Comment" ::
      { "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
      , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
      }
      -> GraphQLType (Maybe Comment)
  , "Post" ::
      { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
      , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
      }
      -> GraphQLType (Maybe Post)
  , "User" ::
      { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
      , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))
      }
      -> GraphQLType (Maybe User)
  }
  ->  { user ::
          { source :: {}
          , args :: { id :: String}
          }
          -> Aff { id :: String}
      }
  -> GraphQLRootType Query
toRootObjectTypeTest =
  toRootObject
  (Proxy :: Proxy Query)


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

