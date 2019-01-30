module GraphQL.Type.Internal.ToInputObject where

import Prelude

import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import GraphQL.Type.Internal (class IsList, class IsListPred, class IsScalar, class IsScalarPred, Id, GraphQLType, nonNull, inputObjectType, toList, toScalar)
import Prim.RowList as RowList
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import Type.Row (class Cons, class Lacks) as Row

-- NOTE different from Row.Utils (class IsRecordPred)
class IsRecordPred a (b :: Bool.Boolean) | a -> b
instance isRecordPredRecord :: IsRecordPred (Record a) Bool.True
else instance isRecordPredMaybeRecord :: IsRecordPred (Maybe (Record a)) Bool.True
else instance isRecordPredOther :: IsRecordPred a Bool.False


class ToInputObjectArgs (i :: # Type) (arg :: # Type) | i -> arg

instance toInputObjectArgsImpl ::
  ( ToInputObjectWithPath "" i o arg
  ) => ToInputObjectArgs i arg


class ToInputObjectWithPath
  (path :: Symbol) (i :: # Type)
  (o :: # Type)
  (arg :: # Type)
  | path i -> o
  , path i -> arg
  where
    toInputObjectWithPath :: SProxy path -> RProxy i -> Record o

instance toInputObjectWithPathToRowList ::
  ( RowList.RowToList i iRl
  , ToInputObjectRowList path iRl o arg
  ) => ToInputObjectWithPath path i o arg
  where
    toInputObjectWithPath _ _
      = Builder.build
          ( toInputObjectRowList
              (SProxy :: SProxy path)
              (RLProxy :: RLProxy iRl)
          )
          {}

class ToInputObjectRowList
  (path :: Symbol) (iRl :: RowList.RowList)
  (to :: # Type)
  (arg :: # Type)
  | path iRl -> to
  , path iRl -> arg
  where
    toInputObjectRowList
      :: SProxy path -> RLProxy iRl
      -> Builder {} (Record to)

instance toInputObjectRowListNil ::
  ToInputObjectRowList path RowList.Nil () ()
  where
    toInputObjectRowList _ _ = identity
else instance toInputObjectRowListCons ::
  ( ToInputObjectType name path typ argType
  , ToInputObjectRowList path restRl restTo restArg
  , Row.Cons name argType restArg arg
  -- Builder.insert
  , Row.Cons name (Record ( "type" :: GraphQLType typ)) restTo to
  , Row.Lacks name restTo
  , Symbol.IsSymbol name
  ) => ToInputObjectRowList path (RowList.Cons name typ restRl) to arg
  where
    toInputObjectRowList pathP _
        = ( Builder.insert
              (SProxy :: SProxy name)
              { "type": toInputObjectType
                          (SProxy :: SProxy name)
                          pathP
                          (Proxy :: Proxy typ)
              }
          )
      <<< ( toInputObjectRowList
              pathP
              (RLProxy :: RLProxy restRl)
          )

class ToInputObjectType
  (name :: Symbol) (path :: Symbol) typ
  argType
  | name path typ -> argType
  where
    toInputObjectType :: SProxy name -> SProxy path -> Proxy typ -> GraphQLType typ

instance toInputObjectFieldDispatch ::
  ( IsScalarPred typ isScalar
  , IsListPred typ isList
  , IsRecordPred typ isRecord
  , ToInputObjectTypeDispatch isScalar isList isRecord name path typ argType
  ) => ToInputObjectType name path typ argType
  where
    toInputObjectType _ _ _
      = toInputObjectTypeDispatch
          (BProxy :: BProxy isScalar)
          (BProxy :: BProxy isList)
          (BProxy :: BProxy isRecord)
          (SProxy :: SProxy name)
          (SProxy :: SProxy path)
          (Proxy :: Proxy typ)

class ToInputObjectTypeDispatch
  (isScalar :: Bool.Boolean) (isList :: Bool.Boolean) (isRecord :: Bool.Boolean)
  (name :: Symbol) (path :: Symbol) typ
  argType
  | isScalar isList isRecord name path typ -> argType
  where
    toInputObjectTypeDispatch
      :: BProxy isScalar -> BProxy isList -> BProxy isRecord
         -> SProxy name -> SProxy path -> Proxy typ -> GraphQLType typ

instance toInputObjectFieldIsScalarId :: -- NOTE Id to String
  ( IsScalar Id
  ) => ToInputObjectTypeDispatch Bool.True Bool.False Bool.False name path Id String
  where
    toInputObjectTypeDispatch _ _ _ _ _ _ = toScalar
else instance toInputObjectFieldIsScalarMaybeId :: -- NOTE Id to String
  ( IsScalar (Maybe Id)
  ) => ToInputObjectTypeDispatch Bool.True Bool.False Bool.False name path (Maybe Id) (Maybe String)
  where
    toInputObjectTypeDispatch _ _ _ _ _ _ = toScalar
else instance toInputObjectFieldIsScalarOther ::
  ( IsScalar typ
  ) => ToInputObjectTypeDispatch Bool.True Bool.False Bool.False name path typ typ
  where
    toInputObjectTypeDispatch _ _ _ _ _ _ = toScalar

instance toInputObjectFieldIsList ::
  ( Symbol.Append name "-Item" name'
  , ToInputObjectType name' path a argType
  , IsList f a
) => ToInputObjectTypeDispatch Bool.False Bool.True Bool.False name path (f a) (f argType)
  where
    toInputObjectTypeDispatch _ _ _ _ _ _
      = let
          item = toInputObjectType
                  (SProxy :: SProxy name')
                  (SProxy :: SProxy path)
                  (Proxy :: Proxy a)
        in
          toList item

instance toInputObjectFieldIsRecord ::
  ( Symbol.Append path "_" path0
  , Symbol.Append path0 name path1
  , ToInputObjectWithPath path1 row o arg -- path1 row -> o arg -- NOTE o is not carried
  , Symbol.IsSymbol path1
  ) => ToInputObjectTypeDispatch Bool.False Bool.False Bool.True name path (Record row) (Record arg)
  where
    toInputObjectTypeDispatch _ _ _ _ _ _
      = let
          fields = toInputObjectWithPath
                    (SProxy :: SProxy path1)
                    (RProxy :: RProxy row)
        in
          nonNull
          ( inputObjectType
              { name: Symbol.reflectSymbol (SProxy :: SProxy path1)
              , fields
              }
          )
else instance toInputObjectFieldIsRecordMaybe ::
  ( Symbol.Append path "_" path0
  , Symbol.Append path0 name path1
  , ToInputObjectWithPath path1 row o arg -- path1 row -> o arg -- NOTE o is not carried
  , Symbol.IsSymbol path1
  ) => ToInputObjectTypeDispatch Bool.False Bool.False Bool.True name path (Maybe (Record row)) (Maybe (Record arg))
  where
    toInputObjectTypeDispatch _ _ _ _ _ _
      = let
        fields = toInputObjectWithPath
                 (SProxy :: SProxy path1)
                 (RProxy :: RProxy row)
        in
          inputObjectType
            { name: Symbol.reflectSymbol (SProxy :: SProxy path1)
            , fields
            }

instance toInputObjectFieldIsNewTypeMaybe ::
  ( Generic typ (Constructor name1 (Argument (Record row)))
  , Symbol.Append path "_" path0
  , Symbol.Append path0 name path1
  , Symbol.Append path1 "-" path2
  , Symbol.Append path2 name1 path3
  , ToInputObjectWithPath path3 row o arg -- path1 row -> o -- NOTE o is not carried
  , Symbol.IsSymbol path3
  ) => ToInputObjectTypeDispatch Bool.False Bool.False Bool.False name path (Maybe typ) (Maybe (Record arg))
  where
    toInputObjectTypeDispatch _ _ _ _ _ _
      = let
        fields = toInputObjectWithPath
                 (SProxy :: SProxy path3)
                 (RProxy :: RProxy row)
        in
          inputObjectType
            { name: Symbol.reflectSymbol (SProxy :: SProxy path3)
            , fields
            }
else instance toInputObjectFieldIsNewType ::
  ( Generic typ (Constructor name1 (Argument (Record row)))
  , Symbol.Append path "_" path0
  , Symbol.Append path0 name path1
  , Symbol.Append path1 "-" path2
  , Symbol.Append path2 name1 path3
  , ToInputObjectWithPath path3 row o arg -- path1 row -> o -- NOTE o is not carried
  , Symbol.IsSymbol path3
  ) => ToInputObjectTypeDispatch Bool.False Bool.False Bool.False name path typ (Record arg)
  where
    toInputObjectTypeDispatch _ _ _ _ _ _
      = let
          fields = toInputObjectWithPath
                    (SProxy :: SProxy path3)
                    (RProxy :: RProxy row)
        in
          nonNull
          ( inputObjectType
              { name: Symbol.reflectSymbol (SProxy :: SProxy path3)
              , fields
              }
          )

{- input

newtype PostDraft = PostDraft
  { id :: Id
  , author :: { id :: Id
              , name :: String
              }
  , content :: { date :: String
               , lines :: Int
               , todoList :: Array
                               { id :: Id
                               , todo :: String
                               }
               }
  }

-}

{- output

G.input
{ name: "PostDraft"
, fields:
  { id: { type: G.id }
  , author:
    { type: G.input
            { name: "PostDraft_author"
            , fields:
              { id: { type: G.id }
              , name: { type: G.string }
              }
            }
    }
  , content:
    { type: G.input
            { name: "PostDraft_content"
            , fields:
              { date: { type: G.string }
              , lines: { type: G.int }
              , todoList: { type: G.list
                                  ( G.input
                                    { name: "PostDraft_content_todoListItem"
                                    , fields:
                                      { id: { type: G.id }
                                      , todo: { type: G.string }
                                      }
                                    }
                                  )
                          }
              }
            }
    }
  }
}

-}

