module MissingDependencies where

import Prelude

import Data.Generic.Rep (class Generic, Constructor)
import Data.Nullable (Nullable)
import GraphQL.Type (GraphQLObject)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Row (RProxy(..))
import Type.Row as Row

-- | class ToDependencies
class ToDependencies (relations :: # Type) (dependencies :: # Type) | relations -> dependencies

instance toDependenciesImpl ::
  ( RowList.RowToList relations relaRl
  , ToDependenciesRowList relaRl depRl
  , Row.ListToRow depRl dependencies
  ) => ToDependencies relations dependencies

class ToDependenciesRowList (relaRl :: RowList) (depRl :: RowList) | relaRl -> depRl

instance toDependenciesRowListBaseCase ::
  ToDependenciesRowList RowList.Nil RowList.Nil
else instance toDependenciesRowListInductionStep ::
  ( ToDependenciesRowList restRelaRl restDepRl
  , FetchTargetType typ targetType
  , Generic targetType (Constructor targetTypeName arg)
  ) => ToDependenciesRowList (RowList.Cons name typ restRelaRl) (RowList.Cons targetTypeName (Unit -> Nullable(GraphQLObject targetType)) restDepRl)

-- | FetchTargetType
class FetchTargetType i o | i -> o
instance fetchTargetTypeFunctor :: FetchTargetType (f t) t
else instance fetchTargetTypeIdentity :: FetchTargetType t t

-- Test

toDependencies :: forall rela deps. ToDependencies rela deps => RProxy rela -> RProxy deps
toDependencies _ = RProxy :: RProxy deps

newtype User = User
  { name :: String
  , posts :: Array Post
  , comments :: Array Comment
  }
derive instance genericUser :: Generic User _

newtype Post = Post
  { id :: String
  , author :: User
  , comments :: Array Comment
  }
derive instance genericPost :: Generic Post _

newtype Comment = Comment
  { id :: String
  , author :: User
  , post :: Post
  }
derive instance genericComment :: Generic Comment _

toDependenciesExample :: RProxy
  ( "Comment" :: Unit -> Nullable (GraphQLObject Comment)
  , "Post" :: Unit -> Nullable (GraphQLObject Post)
  )
toDependenciesExample = toDependencies
                        (RProxy :: RProxy ( posts :: Array Post
                                          , comments :: Array Comment
                                          )
                        )
