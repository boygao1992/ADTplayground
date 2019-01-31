module GraphQL.Type.Internal.ToRootObject where

import GraphQL.Type.Data.Field
import GraphQL.Type.Internal (GraphQLType, GraphQLRootType)
import GraphQL.Type.Internal.ToObject
import Prelude

import Data.Nullable (Nullable)
import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import Prim.RowList as RowList
import Prim.RowList as RowList
import Type.Data.Boolean (BProxy (..))
import Type.Data.Boolean as Bool
import Type.Data.List as List
import Type.Data.Symbol (SProxy (..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy (..))
import Type.Row (RProxy (..))
import Type.Row (RProxy(..))
import Type.Row as Row

-- NOTE differences between ToRootObject and ToObject
-- 1. no source
-- 2. no deps but constructors

-- | ToRootObject
class ToRootObject rootSpec (constructorRow :: # Type) (resolvers :: # Type)
  | rootSpec -> constructorRow
  , rootSpec -> resolvers
  where
    toRootObject ::
      Proxy rootSpec -> Record constructorRow -> Record resolvers
      -> GraphQLRootType rootSpec

-- instance toRootObjectImpl ::
--     -- rootSpec -> rootSpecFl
--   ( Geneirc rootSpec (Constructor rootSpecName (Argument (Record rootSpecRow)))
--   , RowList.RowToList rootSpecRow rootSpecRl
--   , ToFieldList rootSpecRl rootSpecFl
--     -- rootSpecFl -> constructorRow
--   , ToConstructors rootSpecFl constructorRow
--     -- rootSpecFl -> resolvers
--   , ToRootResolvers rootSpecFl resolvers
--   ) => ToRootObject rootSpec resolvers constructorRow

-- | ToConstructors
class ToConstructors
  (rootSpecFl :: List.List) (constructorRow :: # Type)
  | rootSpecFl -> constructorRow

instance toConstructorsImpl ::
  ( FetchRelationalFieldList rootSpecFl rootRelationalSpecList
  , CollectEntitiesTraverse rootRelationalSpecList entityList
  , CollectConstructors entityList constructorRow
  ) => ToConstructors rootSpecFl constructorRow

-- | ToRootResolvers
class ToRootResolvers (fl :: List.List) (resolvers :: # Type) | fl -> resolvers

instance toRootResolversNil ::
  ToRootResolvers List.Nil ()
else instance toRootResolversConsScalarNoArg ::
  ( ToRootResolversDispatch argType typ fieldType target resolve
  , ToRootResolvers restFl restResolvers
  , Row.Cons name resolve restResolvers resolvers
  ) => ToRootResolvers (List.Cons (Field name argType typ fieldType target) restFl) resolvers

class ToRootResolversDispatch
  (argType :: ArgType) typ (fieldType :: FieldType) target
  resolve
  | argType typ fieldType target -> resolve

class ToScalarRootObjectFieldNoArg
  typ resolve (fieldRow :: # Type)
  | typ -> resolve
  , typ -> fieldRow
  where
    toScalarRootObjectFieldNoArg
      :: Proxy typ
      -> resolve
      -> Record fieldRow

class ToScalarRootObjectFieldWithArgs
  (path :: Symbol) (i :: # Type) typ
  resolve
  (fieldRow :: # Type)
  | path i typ -> resolve
  , path i typ -> fieldRow
  where
    toScalarRootObjectFieldWithArgs
      :: SProxy path -> RProxy i -> Proxy typ
      -> resolve
      -> Record fieldRow

class ToScalarRootObjectFieldHandleList typ
  where
    toScalarRootObjectFieldHandleList :: Proxy typ -> GraphQLType typ

class ToScalarRootObjectFieldHandleListDispatch
      (isList :: Bool.Boolean) typ
  where
    toScalarRootObjectFieldHandleListDispatch ::
      BProxy isList -> Proxy typ -> GraphQLType typ


class ToRelationalRootObjectFieldNoArg
  typ target (targetScalars :: # Type)
  resolve
  (fieldRow :: # Type)
  | typ target targetScalars -> resolve
  , typ target targetScalars -> fieldRow
  where
    toRelationalRootObjectFieldNoArg
      :: Proxy typ -> Proxy target -> RProxy targetScalars
      -> resolve
      -> (Unit -> Nullable(GraphQLType (Maybe target)))
      -> Record fieldRow

class ToRelationalRootObjectFieldWithArgs
  (path :: Symbol) (i :: # Type) typ target (targetScalars :: # Type)
  resolve
  (fieldRow :: # Type)
  | i typ target targetScalars -> resolve
  , i typ target targetScalars -> fieldRow
  where
    toRelationalRootObjectFieldWithArgs
      :: SProxy path -> RProxy i -> Proxy typ
         -> Proxy target -> RProxy targetScalars
      -> resolve
      -> (Unit -> Nullable(GraphQLType (Maybe target)))
      -> Record fieldRow

class ToRelationalRootObjectFieldHandleDepList i target o | i target -> o where
  toRelationalRootObjectHandleDepList ::
    Proxy i -> (Unit -> Nullable (GraphQLType (Maybe target))) -> GraphQLType o

class ToRelationalRootObjectFieldHandleDepListDispatch
      (isList :: Bool.Boolean) i target o
  | isList i target -> o
  where
    toRelationalRootObjectFieldHandleDepListDispatch ::
      BProxy isList -> Proxy i -> (Unit -> Nullable (GraphQLType (Maybe target))) -> GraphQLType o

class ToRelationalRootObjectFieldHandleOutputList
  i (targetScalars :: # Type) o
  | i targetScalars -> o

class ToRelationalRootObjectFieldHandleOutputListDispatch
  (isList :: Bool.Boolean) i (targetScalars :: # Type) o
  | isList i targetScalars -> o







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
  , CollectEntitiesTraverseImpl relationalSpecList i' o
  ) => CollectEntitiesImplDispatch Bool.False spec i o

class CollectEntitiesTraverse
  (relationalSpecList :: List.List) (o :: List.List)
  | relationalSpecList -> o

instance collectEntitiesTraverseInit ::
  ( CollectEntitiesTraverseImpl relationalSpecList List.Nil o
  ) => CollectEntitiesTraverse relationalSpecList o

class CollectEntitiesTraverseImpl
  (relationalSpecList :: List.List) (i :: List.List) (o :: List.List)
  | relationalSpecList i -> o

instance collectEntitiesTraverseNil ::
  CollectEntitiesTraverseImpl List.Nil i i
else instance collectEntitiesTraverseCons ::
  ( CollectEntitiesTraverseImpl restFieldList i o'
  , CollectEntitiesImpl spec o' o
  ) => CollectEntitiesTraverseImpl (List.Cons spec restFieldList) i o

-- | CollectConstructors

class CollectConstructors (entityList :: List.List) (constructorRow :: # Type)
  | entityList -> constructorRow

instance collectDependenciesNil ::
  CollectConstructors List.Nil ()
else instance colletDependeciesCons ::
  ( CollectConstructors restFieldList restConstructorRow
  , Generic spec (Constructor specName (Argument (Record specRow)))
  , ToObject spec resolvers deps
  , Row.Cons specName (Record deps -> GraphQLType (Maybe spec)) restConstructorRow constructorRow
  ) => CollectConstructors (List.Cons spec restFieldList) constructorRow


