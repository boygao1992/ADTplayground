module GraphQL.Type.Internal.RootObject where

import GraphQL.Type.Data.Field

import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import GraphQL.Type.Internal (Id)
import GraphQL.Type.Internal.ToObject (class ToFieldList)
import Prim.RowList as RowList
import Type.Data.Boolean as Bool
import Type.Data.List as List
import Type.Row as Row

-- | FetchRelationalFieldList
class FetchRelationalFieldList (fieldList :: List.List) (relationalFieldList :: List.List) | fieldList -> relationalFieldList

instance fetchRelationalFieldListNil ::
  FetchRelationalFieldList List.Nil List.Nil
else instance fetchRelationalFieldListConsScalar ::
  ( FetchRelationalFieldList restList fields
  ) => FetchRelationalFieldList (List.Cons (Field name argType typ ScalarField target) restList) fields
else instance fetchRelationalFieldListConsRelational ::
  ( FetchRelationalFieldList restList restFields
  , List.Set target restFields fields
  ) => FetchRelationalFieldList (List.Cons (Field name argType typ RelationalField target) restList) fields


-- | CollectEntities
class CollectEntities spec (entities :: List.List) | spec -> entities

instance collectEntitiesInit ::
  ( CollectEntitiesImpl spec List.Nil entities
  ) => CollectEntities spec entities

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
  , FetchRelationalFieldList specFl relationalFieldList
  , List.Set spec i i'
  , CollectEntitiesTraverse relationalFieldList i' o
  ) => CollectEntitiesImplDispatch Bool.False spec i o

class CollectEntitiesTraverse
  (relationalFieldList :: List.List) (i :: List.List) (o :: List.List)
  | relationalFieldList i -> o

instance collectEntitiesTraverseNil ::
  CollectEntitiesTraverse List.Nil i i
else instance collectEntitiesTraverseCons ::
  ( CollectEntitiesTraverse restList i o'
  , CollectEntitiesImpl spec o' o
  ) => CollectEntitiesTraverse (List.Cons spec restList) i o
