module GraphQL.Type.Internal.ToRootObject where

import GraphQL.Type.Data.Field

import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe (..))
import Data.Maybe (Maybe)
import GraphQL.Type.Internal (Id, GraphQLType)
import GraphQL.Type.Internal.ToObject (class ToFieldList, class ToObject)
import Prim.RowList as RowList
import Type.Data.Boolean as Bool
import Type.Data.List (LProxy (..))
import Type.Data.List as List
import Type.Row (RProxy (..))
import Type.Row as Row
import Examples.ForumExample.Model

-- | FetchRelationalFieldList
class FetchRelationalFieldList (fieldList :: List.List) (relationalSpecList :: List.List) | fieldList -> relationalSpecList

instance fetchRelationalFieldListNil ::
  FetchRelationalFieldList List.Nil List.Nil
else instance fetchRelationalFieldListConsScalar ::
  ( FetchRelationalFieldList restFieldList specList
  ) => FetchRelationalFieldList (List.Cons (Field name argType typ ScalarField target) restFieldList) specList
else instance fetchRelationalFieldListConsRelational ::
  ( FetchRelationalFieldList restFieldList restSpecList
  , List.Set target restSpecList specList
  ) => FetchRelationalFieldList (List.Cons (Field name argType typ RelationalField target) restFieldList) specList


-- | CollectEntities
class CollectEntities spec (entityList :: List.List) | spec -> entityList

instance collectEntitiesInit ::
  ( CollectEntitiesImpl spec List.Nil entityList
  ) => CollectEntities spec entityList

class CollectEntitiesImpl spec (i :: List.List) (o :: List.List) | spec i -> o

instance collectEntitiesImplContainsPred ::
  ( List.ContainsPred i spec contains
  , CollectEntitiesImplDispatch contains spec i o
  ) => CollectEntitiesImpl spec i o

class CollectEntitiesImplDispatch
  (contains :: Bool.Boolean) spec (i :: List.List)
  (o :: List.List)
  | contains spec i -> o

instance collectEntitiesImplDoesContain ::
  CollectEntitiesImplDispatch Bool.True spec i i

instance collectEntitiesImplNotContain ::
  ( Generic spec (Constructor specName (Argument (Record specRow)))
  , RowList.RowToList specRow specRl
  , ToFieldList specRl specFl
  , FetchRelationalFieldList specFl relationalSpecList
  , List.Set spec i i'
  , CollectEntitiesTraverse relationalSpecList i' o
  ) => CollectEntitiesImplDispatch Bool.False spec i o

class CollectEntitiesTraverse
  (relationalSpecList :: List.List) (i :: List.List) (o :: List.List)
  | relationalSpecList i -> o

instance collectEntitiesTraverseNil ::
  CollectEntitiesTraverse List.Nil i i
else instance collectEntitiesTraverseCons ::
  ( CollectEntitiesTraverse restFieldList i o'
  , CollectEntitiesImpl spec o' o
  ) => CollectEntitiesTraverse (List.Cons spec restFieldList) i o

-- | CollectConstructors

class CollectConstructors (entityList :: List.List) (entityDeps :: # Type)
  | entityList -> entityDeps

instance collectDependenciesNil ::
  CollectConstructors List.Nil ()
else instance colletDependeciesCons ::
  ( CollectConstructors restFieldList restEntityDeps
  , Generic spec (Constructor specName (Argument (Record specRow)))
  , ToObject spec resolvers deps
  , Row.Cons specName (Record deps -> GraphQLType (Maybe spec)) restEntityDeps entityDeps
  ) => CollectConstructors (List.Cons spec restFieldList) entityDeps

