module GraphQL.Type.Internal.ToRootObject where

import Prelude

import Control.Monad.ST (ST)
import Type.Proxy (Proxy (..))
import Control.Monad.ST (run) as ST
import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null, notNull)
import GraphQL.Type.Data.Field (Field, RelationalField, ScalarField)
import GraphQL.Type.Internal (GraphQLRootType, GraphQLType, rootObjectType)
import GraphQL.Type.Internal.ToObject (class ToFieldList, class ToObject, class ToObjectRow, class ToResolvers, toObjectRow)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.ST (STRecord)
import Record.ST (modify, thaw) as RST
import Record.ST.Nested (peekLazyRef) as RST
import Type.Data.Boolean as Bool
import Type.Data.List (LProxy(..))
import Type.Data.List as List
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Row (RProxy(..))
import Type.Row as Row

-- NOTE differences between ToRootObject and ToObject
-- 1. no source
-- 2. no deps but constructors

-- | ToRootObject
class ToRootObject rootSpec (rootSource :: Type) (constructorRow :: # Type) (resolverRow :: # Type)
  | rootSpec -> constructorRow
  , rootSpec rootSource -> resolverRow
  where
    toRootObject ::
      Proxy rootSpec -> Record constructorRow
      -> Proxy rootSource -> Record resolverRow
      -> GraphQLRootType rootSpec rootSource

instance toRootObjectImpl ::
    -- rootSpec -> rootSpecFl
  ( Generic rootSpec (Constructor rootSpecName (Argument (Record rootSpecRow)))
  , RowList.RowToList rootSpecRow rootSpecRl
  , ToFieldList rootSpecRl rootSpecFl
    -- rootSpecFl -> entityList depRow
  , FetchRelationalFieldList rootSpecFl rootRelationalSpecList
  , CollectEntitiesTraverse rootRelationalSpecList entityList
  , CollectConstructors entityList constructorRow
  , ToEntityObjects entityList constructorRow depRow
    -- rootSpecFl -> resolverRow
  , ToResolvers rootSource rootSpecFl resolverRow -- NOTE source = ()
  , ToObjectRow rootSpecFl rootSpecName rootSource resolverRow depRow to -- NOTE source = ()
  , Symbol.IsSymbol rootSpecName
  ) => ToRootObject rootSpec rootSource constructorRow resolverRow
  where
    toRootObject _ constructors rootSource resolvers =
      let
        deps = toEntityObjects
                (LProxy :: LProxy entityList)
                constructors
      in
        rootObjectType
          ( { name: Symbol.reflectSymbol (SProxy :: SProxy rootSpecName)
            , fields:
                Builder.build
                ( toObjectRow -- NOTE reuse toObjectRow with an empty source for now
                  (LProxy :: LProxy rootSpecFl)
                  (SProxy :: SProxy rootSpecName)
                  (Proxy :: Proxy rootSource)
                  resolvers
                  deps
                )
                {}
            }
          )

-- class ToRootObjectRow
--   (specFl :: List.List) (specName :: Symbol) (resolverRow :: # Type) (depRow :: # Type)
--   (to :: # Type)
--   | specFl specName resolverRow depRow -> to
--   where
--     toRootObjectRow :: LProxy specFl -> SProxy specName -> Record resolverRow -> Record depRow -> Builder {} (Record to)

-- instance toRootObjectRowNil ::
--   ToRootObjectRow List.Nil specName resolverRow depRow ()
--   where
--     toRootObjectRow _ _ _ _ = identity

-- instance toRootObjectRowCons ::
--   ( ToRootObjectRow restFl specName resolverRow depRow restTo
--   , ToRootObjectRowDispatch specName fieldName argType typ fieldType target resolverRow depRow fieldRow
--   , Row.Cons fieldName (Record fieldRow) restTo to
--   , Row.Lacks fieldName restTo
--   , Symbol.IsSymbol fieldName
--   ) => ToRootObjectRow (List.Cons (Field fieldName argType typ fieldType target) restFl) specName resolverRow depRow to
--   where
--     toRootObjectRow _ _ resolvers deps =
--           Builder.insert
--             (SProxy :: SProxy fieldName)
--             ( toRootObjectRowDispatch
--                 (SProxy :: SProxy specName)
--                 (SProxy :: SProxy fieldName)
--                 (AProxy :: AProxy argType)
--                 (Proxy :: Proxy typ)
--                 (FTProxy :: FTProxy fieldType)
--                 (Proxy :: Proxy target)
--                 resolvers
--                 deps
--             )
--       <<< toRootObjectRow
--             (LProxy :: LProxy restFl)
--             (SProxy :: SProxy specName)
--             resolvers
--             deps

-- class ToRootObjectRowDispatch
--   (specName :: Symbol) (fieldName :: Symbol) (argType :: ArgType) typ (fieldType :: FieldType) target (resolvers :: # Type) (deps :: # Type)
--   (fieldRow :: # Type)
--   | specName fieldName argType typ fieldType target resolvers deps -> fieldRow
--   where
--     toRootObjectRowDispatch
--       :: SProxy specName -> SProxy fieldName -> AProxy argType -> Proxy typ
--          -> FTProxy fieldType -> Proxy target
--       -> Record resolvers -> Record deps
--       -> Record fieldRow

-- instance toRootObjectRowScalarNoArg ::
--   ( ToRootObjectScalarFieldNoArg typ resolve fieldRow
--   , Row.Cons fieldName resolve restResolverRow resolverRow
--   , Symbol.IsSymbol fieldName
--   ) => ToRootObjectRowDispatch specName fieldName NoArg typ ScalarField target resolverRow depRow fieldRow
--   where
--     toRootObjectRowDispatch _ _ _ _ _ _ resolvers _ =
--       toRootObjectScalarFieldNoArg
--       (Proxy :: Proxy typ)
--       (Record.get (SProxy :: SProxy fieldName) resolvers)

-- instance toRootObjectRowScalarWithArgs ::
--   ( Symbol.Append specName "_" path0
--   , Symbol.Append path0 fieldName path
--   , ToRootObjectScalarFieldWithArgs path i typ resolve fieldRow
--   , Symbol.IsSymbol fieldName
--   , Row.Cons fieldName resolve restResolvers resolverRow
--   ) => ToRootObjectRowDispatch specName fieldName (WithArgs (Record i)) typ ScalarField target resolverRow depRow fieldRow
--   where
--     toRootObjectRowDispatch _ _ _ _ _ _ resolvers _ =
--       toRootObjectScalarFieldWithArgs
--       (SProxy :: SProxy path)
--       (RProxy :: RProxy i)
--       (Proxy :: Proxy typ)
--       (Record.get (SProxy :: SProxy fieldName) resolvers)

-- instance toRootObjectRowRelationalNoArgs ::
--   ( Generic target (Constructor targetName (Argument (Record targetRow)))
--   , RowList.RowToList targetRow targetRl
--   , ToFieldList targetRl targetFl
--   , FetchScalarFields targetFl targetScalars
--   , ToRootObjectRelationalFieldNoArg typ target targetScalars resolve fieldRow
--   , Symbol.IsSymbol name
--   , Row.Cons name resolve restResolvers resolverRow
--   , Symbol.IsSymbol targetName
--   , Row.Cons targetName (Unit -> Nullable(GraphQLType (Maybe target))) restDepRow depRow
--   ) => ToRootObjectRowDispatch specName name NoArg typ RelationalField target resolverRow depRow fieldRow
--   where
--     toRootObjectRowDispatch _ _ _ _ _ _ resolvers deps =
--       toRootObjectRelationalFieldNoArg
--       (Proxy :: Proxy typ)
--       (Proxy :: Proxy target)
--       (RProxy :: RProxy targetScalars)
--       (Record.get (SProxy :: SProxy name) resolvers)
--       (Record.get (SProxy :: SProxy targetName) deps)

-- instance toRootObjectRowRelationalWithArgs ::
--   ( Generic target (Constructor targetName (Argument (Record targetRow)))
--   , RowList.RowToList targetRow targetRl
--   , ToFieldList targetRl targetFl
--   , FetchScalarFields targetFl targetScalars
--   , Symbol.Append specName "_" path0
--   , Symbol.Append path0 name path
--   , ToRootObjectRelationalFieldWithArgs path i typ target targetScalars resolve fieldRow
--   , Symbol.IsSymbol name
--   , Row.Cons name resolve restResolvers resolvers
--   , Symbol.IsSymbol targetName
--   , Row.Cons targetName (Unit -> Nullable(GraphQLType (Maybe target))) restDeps deps
--   ) => ToRootObjectRowDispatch specName name (WithArgs (Record i)) typ RelationalField target resolvers deps fieldRow
--   where
--     toRootObjectRowDispatch _ _ _ _ _ _ rs ds =
--       toRootObjectRelationalFieldWithArgs
--         (SProxy :: SProxy path)
--         (RProxy :: RProxy i)
--         (Proxy :: Proxy typ)
--         (Proxy :: Proxy target)
--         (RProxy :: RProxy targetScalars)
--         (Record.get (SProxy :: SProxy name) rs)
--         (Record.get (SProxy :: SProxy targetName) ds)

-- -- | ToRootResolvers
-- class ToRootResolvers (fl :: List.List) (resolverRow :: # Type) | fl -> resolverRow

-- instance toRootResolversNil ::
--   ToRootResolvers List.Nil ()

-- instance toRootResolversCons ::
--   ( ToRootResolversDispatch argType typ fieldType target resolve
--   , ToRootResolvers restFl restResolverRow
--   , Row.Cons name resolve restResolverRow resolverRow
--   ) => ToRootResolvers (List.Cons (Field name argType typ fieldType target) restFl) resolverRow

-- class ToRootResolversDispatch
--   (argType :: ArgType) typ (fieldType :: FieldType) target resolve
--   | argType typ fieldType target -> resolve

-- instance toRootResolversScalarNoArg ::
--   ( ToRootObjectScalarFieldNoArg typ resolve fieldRow
--   ) => ToRootResolversDispatch NoArg typ ScalarField target resolve

-- instance toRootResolversScalarWithArgs ::
--   ( ToRootObjectScalarFieldWithArgs "" i typ resolve fieldRow
--   ) => ToRootResolversDispatch (WithArgs (Record i)) typ ScalarField target resolve

-- instance toRootResolversRelationalNoArg ::
--   ( Generic target (Constructor targetName (Argument (Record targetRow)))
--   , RowList.RowToList targetRow targetRl
--   , ToFieldList targetRl targetFl
--   , FetchScalarFields targetFl targetScalars
--   , ToRootObjectRelationalFieldNoArg typ target targetScalars resolve fieldRow
--   ) => ToRootResolversDispatch NoArg typ RelationalField target resolve

-- instance toRootResolversRelationalWithArgs ::
--   ( Generic target (Constructor targetName (Argument (Record targetRow)))
--   , RowList.RowToList targetRow targetRl
--   , ToFieldList targetRl targetFl
--   , FetchScalarFields targetFl targetScalars
--   , ToRootObjectRelationalFieldWithArgs "" i typ target targetScalars resolve fieldRow
--   ) => ToRootResolversDispatch (WithArgs (Record i)) typ RelationalField target resolve

-- -- | ToRootObjectScalarField
-- class ToRootObjectScalarFieldNoArg
--   typ resolve (fieldRow :: # Type)
--   | typ -> resolve
--   , typ -> fieldRow
--   where
--     toRootObjectScalarFieldNoArg :: Proxy typ -> resolve -> Record fieldRow

-- class ToRootObjectScalarFieldWithArgs
--   (path :: Symbol) (i :: # Type) typ
--   resolve
--   (fieldRow :: # Type)
--   | path i typ -> resolve
--   , path i typ -> fieldRow
--   where
--     toRootObjectScalarFieldWithArgs
--       :: SProxy path -> RProxy i -> Proxy typ
--       -> resolve
--       -> Record fieldRow

-- -- | ToRootObjectRelationalField
-- class ToRootObjectRelationalFieldNoArg
--   typ target (targetScalars :: # Type)
--   resolve
--   (fieldRow :: # Type)
--   | typ target targetScalars -> resolve
--   , typ target targetScalars -> fieldRow
--   where
--     toRootObjectRelationalFieldNoArg
--       :: Proxy typ -> Proxy target -> RProxy targetScalars
--       -> resolve
--       -> (Unit -> Nullable(GraphQLType (Maybe target)))
--       -> Record fieldRow

-- class ToRootObjectRelationalFieldWithArgs
--   (path :: Symbol) (i :: # Type) typ target (targetScalars :: # Type)
--   resolve
--   (fieldRow :: # Type)
--   | i typ target targetScalars -> resolve
--   , i typ target targetScalars -> fieldRow
--   where
--     toRootObjectRelationalFieldWithArgs
--       :: SProxy path -> RProxy i -> Proxy typ
--          -> Proxy target -> RProxy targetScalars
--       -> resolve
--       -> (Unit -> Nullable(GraphQLType (Maybe target)))
--       -> Record fieldRow

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
  , Row.Cons name dep restObjectRow objectRow
  , Row.Cons name (Unit -> dep) restTo to
  , Row.Lacks name restTo
  , Symbol.IsSymbol name
  ) => DependencyRecordFold (RowList.Cons name dep restRl) objectRow to
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


