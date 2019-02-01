module GraphQL.Type.Internal.ToRootObject where

import GraphQL.Type.Data.Field (Field, RelationalField, ScalarField)
import GraphQL.Type.Internal (GraphQLType, GraphQLRootType)
import GraphQL.Type.Internal.ToObject (class ToFieldList, class ToObject)
import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST (run) as ST
import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null, notNull)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.ST (STRecord)
import Record.ST (freeze, modify, thaw) as RST
import Record.ST.Nested (peekLazyRef) as RST
import Type.Data.Boolean as Bool
import Type.Data.List (LProxy (..))
import Type.Data.List as List
import Type.Data.RowList (RLProxy (..))
import Type.Data.Symbol (SProxy (..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy)
import Type.Row (RProxy (..))
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

-- TODO
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
class ToEntityObjects (entityList :: List.List) (constructorRow :: # Type) (depRow :: # Type)
  | entityList constructorRow -> depRow
  where
    toEntityObjects :: LProxy entityList -> Record constructorRow -> Record depRow

instance toEntityObjectsImpl ::
  ( InitObjectRecord entityList objectRow
  , DependencyRecord objectRow depRow
  , PopulateObjectRecord entityList constructorRow depRow objectRow
  ) => ToEntityObjects entityList constructorRow depRow
  where
    toEntityObjects _ constructors = ST.run do
      objects <- initObjectRecord (LProxy :: LProxy entityList)
      deps <- dependencyRecord objects
      populateObjectRecord
        (LProxy :: LProxy entityList)
        constructors
        deps
        objects
      pure deps

-- | InitObjectRecord
class InitObjectRecord
  (entityList :: List.List) (objectRow :: # Type)
  | entityList -> objectRow
  where
    initObjectRecord :: forall h. LProxy entityList -> ST h (STRecord h objectRow)

instance initObjectRecordImpl ::
  ( InitObjectRecordFold entityList objectRow
  ) => InitObjectRecord entityList objectRow
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
  (objectRow :: # Type) (depRow :: # Type)
  | objectRow -> depRow
  where
    dependencyRecord :: forall h. STRecord h objectRow -> ST h (Record depRow)

instance dependencyRecordImpl ::
  ( RowList.RowToList objectRow objectRl
  , DependencyRecordFold objectRl objectRow depRow
  ) => DependencyRecord objectRow depRow
  where
    dependencyRecord objects = do
      builder <- dependencyRecordFold (RLProxy :: RLProxy objectRl) objects
      pure ( Builder.build builder {} )

class DependencyRecordFold
  (rl :: RowList) (objectRow :: # Type) (to :: # Type)
  | rl objectRow -> to
  where
    dependencyRecordFold :: forall h. RLProxy rl -> STRecord h objectRow -> ST h (Builder {} (Record to))

instance dependencyRecordFoldNil ::
  DependencyRecordFold RowList.Nil objectRow ()
  where
    dependencyRecordFold _ _ = pure identity

instance dependencyRecordFoldCons ::
  ( DependencyRecordFold restRl objectRow restTo
  , Row.Cons name (Nullable (GraphQLType (Maybe spec))) restObjectRow objectRow
  , Row.Cons name (Unit -> Nullable (GraphQLType (Maybe spec))) restTo to
  , Row.Lacks name restTo
  , Symbol.IsSymbol name
  ) => DependencyRecordFold (RowList.Cons name spec restRl) objectRow to
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
  (entityList :: List.List) (constructorRow :: # Type) (depRow :: # Type) (objectRow :: # Type)
  where
    populateObjectRecord :: forall h. LProxy entityList -> Record constructorRow -> Record depRow -> STRecord h objectRow -> ST h Unit

instance populateObjectRecordNil ::
  PopulateObjectRecord List.Nil constructorRow depRow objectRow
  where
    populateObjectRecord _ _ _ _ = pure unit

instance populateObjectRecordCons ::
  ( Generic spec (Constructor name argument)
  , PopulateObjectRecord restList constructorRow depRow objectRow
  , Row.Cons name ((Record argRow) -> GraphQLType (Maybe spec)) restConstructorRow constructorRow
  , ConstructorDependency argRow depRow
  , Row.Cons name (Nullable (GraphQLType (Maybe spec))) restObjectRow objectRow
  , Symbol.IsSymbol name
  ) => PopulateObjectRecord (List.Cons spec restList) constructorRow depRow objectRow
  where
    populateObjectRecord _ constructors deps objects = do
      populateObjectRecord
        (LProxy :: LProxy restList)
        constructors
        deps
        objects
      let constructor = Record.get (SProxy :: SProxy name) constructors
          args = constructorDependency
                   (RProxy :: RProxy argRow)
                   deps
          gType = constructor args
      RST.modify (SProxy :: SProxy name) (const (notNull gType)) objects

-- instance populateObjectRecordNil ::
--   PopulateObjectRecord RowList.Nil h constructorRow depRow objectRow
--   where
--     populateObjectRecord _ _ _ objects = pure objects
-- instance populateObjectRecordCons ::
--   ( PopulateObjectRecord restRl h constructorRow depRow objectRow
--   , Row.Cons name constructor restConstructorRow constructorRow
--   , ConstructorDependency constructor depRow subset
--   , Symbol.IsSymbol name
--   ) => PopulateObjectRecord (RowList.Cons name constructor restRl) h constructorRow depRow objectRow
  -- where
  --   populateObjectRecord _ constructors deps objects = do
  --     rest <- populateObjectRecord
  --               (RLProxy :: RLProxy restRl)
  --               constructors
  --               deps
  --               objects
  --     let constructor = Record.get (SProxy :: SProxy name) constructors
  --         dep = constructorDependency
  --                 (Proxy :: Proxy constructor)
  --                 deps
  --         gType = constructor dep
  --     RST.modify (SProxy :: SProxy name) (const (notNull gType)) rest
  --     pure objects

---- | ConstructorDependency
class ConstructorDependency (argRow :: # Type) (depRow :: # Type)
  where
    constructorDependency :: RProxy argRow -> Record depRow -> Record argRow

instance constructorDependencyImpl ::
  ( RowList.RowToList argRow argRl
  , ConstructorDependencyFold argRl depRow argRow
  ) => ConstructorDependency argRow depRow
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


