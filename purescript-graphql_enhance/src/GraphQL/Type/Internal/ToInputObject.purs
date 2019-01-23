module GraphQL.Type.Internal.ToInputObject where

import GraphQL.Type.Internal

import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Newtype (class Newtype)
import Prim.RowList as RowList
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import Type.Row (class Cons) as Row
import Type.Row.Utils (class IsRecordPred) as Row

class ToInputObject (i :: # Type) (o :: # Type) | i -> o where
  toInputObject :: RProxy i -> Record o

instance toInputObjectInit ::
  ( ToInputObjectWithPath "" i o
  ) => ToInputObject i o

class ToInputObjectWithPath (path :: Symbol) (i :: # Type) (o :: # Type) | path i -> o where
  toInputObjectWithPath :: SProxy path -> RProxy i -> Record o

instance toInputObjectRowList ::
  ( RowList.RowToList i iRl
  , ToInputObjectRowList path iRl o
  ) => ToInputObjectWithPath path i o

class ToInputObjectRowList (path :: Symbol) (iRl :: RowList.RowList) (o :: # Type) | path iRl -> o where
  toInputObjectRowList :: SProxy path -> RLProxy iRl -> Record o

instance toInputObjectRowListNil ::
  ToInputObjectRowList path RowList.Nil ()
else instance toInputObjectRowListCons ::
  ( ToInputObjectType name path typ gType
  , Row.Cons "type" gType () typeRow
  , ToInputObjectRowList path restRl restO
  , Row.Cons name (Record typeRow) restO o
  ) => ToInputObjectRowList path (RowList.Cons name typ restRl) o

class ToInputObjectType (name :: Symbol) (path :: Symbol) typ gType | name path typ -> gType where
  toInputObjectType :: SProxy name -> SProxy path -> Proxy typ -> gType

instance toInputObjectFieldDispatch ::
  ( IsScalarPred typ isScalar
  , IsListPred typ isList
  , Row.IsRecordPred typ isRecord
  , ToInputObjectTypeDispatch isScalar isList isRecord name path typ gType
  ) => ToInputObjectType name path typ gType

class ToInputObjectTypeDispatch
  (isScalar :: Bool.Boolean) (isList :: Bool.Boolean) (isRecord :: Bool.Boolean)
  (name :: Symbol) (path :: Symbol) typ gType
  | isScalar isList isRecord name path typ -> gType
  where
    toInputObjectTypeDispatch :: BProxy isScalar -> BProxy isList -> BProxy isRecord -> SProxy name -> SProxy path -> Proxy typ -> gType

instance toInputObjectFieldIsScalar ::
  ( IsScalar typ
  ) => ToInputObjectTypeDispatch Bool.True isList isRecord name path typ (GraphQLType typ)
  where
    toInputObjectTypeDispatch _ _ _ _ _ _ = toScalar
else instance toInputObjectFieldIsList ::
  ( Symbol.Append name "Item" name'
  , ToInputObjectType name' path a gType
  , IsList (f a)
  ) => ToInputObjectTypeDispatch Bool.False Bool.True isRecord name path (f a) (GraphQLType (f a))
  where
    toInputObjectTypeDispatch _ _ _ _ _ _
      = toList
        ( toInputObjectType
          (SProxy :: SProxy name')
          (SProxy :: SProxy path)
          (Proxy :: Proxy typ)
        )
else instance toInputObjectFieldIsRecord ::
  ( Symbol.Append path "_" path0
  , Symbol.Append path0 name path1
  , ToInputObjectWithPath path1 row o -- path1 row -> o
  , Row.Cons "name" String () row0
  , Row.Cons "fields" (Record o) row0 row1 -- TODO for value-level
  ) => ToInputObjectTypeDispatch Bool.False Bool.False Bool.True name path (Record row) (GraphQLType (Record row))
else instance toInputObjectFieldIsNewType ::
  ( Newtype typ a
  , Generic typ (Constructor name1 (Argument (Record row))) -- NOTE use name1 over name
  , Symbol.Append path "_" path0
  , Symbol.Append path0 name1 path1
  , ToInputObjectWithPath path1 row o -- path1 row -> o
  , Row.Cons "name" String () row0
  , Row.Cons "fields" (Record o) row0 row1 -- TODO for value-level
  ) => ToInputObjectTypeDispatch Bool.False Bool.False Bool.False name path typ (GraphQLType (Record row))

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
