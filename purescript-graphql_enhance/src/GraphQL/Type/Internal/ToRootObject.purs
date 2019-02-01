module GraphQL.Type.Internal.ToRootObject where

import GraphQL.Type.Data.Field
import GraphQL.Type.Internal (GraphQLType, GraphQLRootType)
import GraphQL.Type.Internal.ToObject
import Prelude

import Control.Monad.ST (kind Region, ST)
import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.ST (STRecord)
import Record.ST as RST
import Record.ST.Nested as RST
import Type.Data.Boolean (BProxy (..))
import Type.Data.Boolean as Bool
import Type.Data.List (LProxy (..))
import Type.Data.List as List
import Type.Data.RowList (RLProxy (..))
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
class ToRootObject rootSpec (constructorRow :: # Type) (resolverRow :: # Type)
  | rootSpec -> constructorRow
  , rootSpec -> resolverRow
  where
    toRootObject ::
      Proxy rootSpec -> Record constructorRow -> Record resolverRow
      -> GraphQLRootType rootSpec

-- instance toRootObjectImpl ::
--     -- rootSpec -> rootSpecFl
--   ( Generic rootSpec (Constructor rootSpecName (Argument (Record rootSpecRow)))
--   , RowList.RowToList rootSpecRow rootSpecRl
--   , ToFieldList rootSpecRl rootSpecFl
--     -- rootSpecFl -> constructorRow
--   , ToConstructors rootSpecFl constructorRow
--     -- rootSpecFl -> resolverRow
--   , ToResolvers () rootSpecFl resolverRow
--   ) => ToRootObject rootSpec resolverRow constructorRow
--   where
--     toRootObject _ constructors resolverRow =

-- | ToEntityObjects
class ToEntityObjects (entityList :: List.List) (constructorRow :: # Type) (objectRow :: # Type)
  | entityList constructorRow -> objectRow
  where
    toEntityObjects :: LProxy entityList -> Record constructorRow -> Record objectRow

-- | InitObjectRecord
class InitObjectRecord
  (h :: Region) (entityList :: List.List) (objectRow :: # Type)
  | h entityList -> objectRow
  where
    initObjectRecord :: LProxy entityList -> ST h (STRecord h objectRow)

instance initObjectRecordImpl ::
  ( InitObjectRecordFold entityList objectRow
  ) => InitObjectRecord h entityList objectRow
  where
    initObjectRecord _ =
      RST.thaw
        ( Builder.build
            (initObjectRecordFold (LProxy :: LProxy entityList))
            {}
        )

class InitObjectRecordFold
  (list :: List.List) (to :: # Type)
  | list -> to
  where
    initObjectRecordFold ::
      LProxy list -> Builder {} (Record to)

instance initObjectRecordFoldNil ::
  InitObjectRecordFold List.Nil ()
  where
    initObjectRecordFold _ = identity

instance initObjectRecordFoldCons ::
  ( InitObjectRecordFold restList restTo
  , Generic spec (Constructor specName arg)
  , Row.Cons specName (Nullable (GraphQLType (Maybe spec))) restTo to
  , Row.Lacks specName restTo
  , Symbol.IsSymbol specName
  ) => InitObjectRecordFold (List.Cons spec restList) to
  where
    initObjectRecordFold _ =
          Builder.insert
            (SProxy :: SProxy specName)
            null
      <<< initObjectRecordFold
            (LProxy :: LProxy restList)

-- | DependencyRecord
class DependencyRecord
  (h :: Region) (objectRow :: # Type) (depRow :: # Type)
  | h objectRow -> depRow
  where
    dependencyRecord :: STRecord h objectRow -> ST h (Record depRow)

instance dependencyRecordImpl ::
  ( RowList.RowToList objectRow objectRl
  , DependencyRecordFold objectRl h objectRow depRow
  ) => DependencyRecord h objectRow depRow
  where
    dependencyRecord objects = do
      builder <- dependencyRecordFold (RLProxy :: RLProxy objectRl) objects
      pure ( Builder.build builder {} )

class DependencyRecordFold
  (rl :: RowList) (h :: Region) (objectRow :: # Type) (to :: # Type)
  | rl h objectRow -> to
  where
    dependencyRecordFold :: RLProxy rl -> STRecord h objectRow -> ST h (Builder {} (Record to))

instance dependencyRecordFoldNil ::
  DependencyRecordFold RowList.Nil h objectRow ()
  where
    dependencyRecordFold _ _ = pure identity

instance dependencyRecordFoldCons ::
  ( DependencyRecordFold restRl h objectRow restTo
  , Row.Cons name (Nullable (GraphQLType (Maybe spec))) restObjectRow objectRow
  , Row.Cons name (Unit -> Nullable (GraphQLType (Maybe spec))) restTo to
  , Row.Lacks name restTo
  , Symbol.IsSymbol name
  ) => DependencyRecordFold (RowList.Cons name spec restRl) h objectRow to
  where
    dependencyRecordFold _ objects = do
      restBuilder <- dependencyRecordFold (RLProxy :: RLProxy restRl) objects
      ref <- RST.peekLazyRef (SProxy :: SProxy name) objects
      pure
        (     Builder.insert
                (SProxy :: SProxy name)
                ref
          <<< restBuilder
        )

-- | PopulateObjectRecord
class PopulateObjectRecord
  (h :: Region) (depRow :: # Type) (objectRow :: # Type)
  where
    populateObjectRecord :: Record depRow -> STRecord h objectRow -> ST h (STRecord h objectRow)

---- | ConstructorDependency
class ConstructorDependency constructor (depRow :: # Type) (subset :: # Type)
  | constructor -> subset
  where
    constructorDependency :: Proxy constructor -> Record depRow -> Record subset

instance constructorDependencyImpl ::
  ( RowList.RowToList argRow argRl
  , ConstructorDependencyFold argRl depRow subset
  ) => ConstructorDependency ((Record argRow) -> o) depRow subset
  where
    constructorDependency _ depRecord =
      Builder.build
        ( constructorDependencyFold
          (RLProxy :: RLProxy argRl)
          depRecord
        )
        {}

class ConstructorDependencyFold
  (argRl :: RowList) (depRow :: # Type) (to :: # Type)
  | argRl depRow -> to
  where
    constructorDependencyFold :: RLProxy argRl -> Record depRow -> Builder {} (Record to)

instance constructorDependencyFoldNil ::
  ConstructorDependencyFold RowList.Nil depRow ()
  where
    constructorDependencyFold _ _ = identity

instance constructorDependencyFoldCons ::
  ( ConstructorDependencyFold restRl depRow restTo
  , Row.Cons name dep restDepRow depRow
  , Row.Cons name dep restTo to
  , Row.Lacks name restTo
  , Symbol.IsSymbol name
  ) => ConstructorDependencyFold (RowList.Cons name dep restRl) depRow to
  where
    constructorDependencyFold _ depRecord =
          Builder.insert
            (SProxy :: SProxy name)
            (Record.get (SProxy :: SProxy name) depRecord)
      <<< constructorDependencyFold
            (RLProxy :: RLProxy restRl)
            depRecord


-- | ToConstructors
class ToConstructors
  (rootSpecFl :: List.List) (constructorRow :: # Type)
  | rootSpecFl -> constructorRow

instance toConstructorsImpl ::
  ( FetchRelationalFieldList rootSpecFl rootRelationalSpecList
  , CollectEntitiesTraverse rootRelationalSpecList entityList
  , CollectConstructors entityList constructorRow
  ) => ToConstructors rootSpecFl constructorRow


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
  , ToObject spec resolverRow deps
  , Row.Cons specName (Record deps -> GraphQLType (Maybe spec)) restConstructorRow constructorRow
  ) => CollectConstructors (List.Cons spec restFieldList) constructorRow


