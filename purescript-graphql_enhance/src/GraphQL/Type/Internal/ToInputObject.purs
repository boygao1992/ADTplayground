module GraphQL.Type.Internal.ToInputObject where

import Prelude

import Data.Generic.Rep (class Generic, Constructor, Argument)
import GraphQL.Type.Internal (class IsList, class IsListPred, class IsScalar, class IsScalarPred, GraphQLType, inputObjectType, toList, toScalar)
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
import Type.Row.Utils (class IsRecordPred) as Row

-- class ToInputObject (i :: # Type) (o :: # Type) | i -> o where
--   toInputObject :: RProxy i -> Record o

-- instance toInputObjectInit ::
--   ( ToInputObjectWithPath "" i o
--   ) => ToInputObject i o
--   where
--     toInputObject i = toInputObjectWithPath (SProxy :: SProxy "") i

class ToInputObjectWithPath (path :: Symbol) (i :: # Type) (o :: # Type) | path i -> o where
  toInputObjectWithPath :: SProxy path -> RProxy i -> Record o

instance toInputObjectWithPathToRowList ::
  ( RowList.RowToList i iRl
  , ToInputObjectRowList path iRl () o
  ) => ToInputObjectWithPath path i o
  where
    toInputObjectWithPath _ _
      = Builder.build
          ( toInputObjectRowList
              (SProxy :: SProxy path)
              (RLProxy :: RLProxy iRl)
          )
          {}

class ToInputObjectRowList (path :: Symbol) (iRl :: RowList.RowList) (from :: # Type) (to :: # Type) | path iRl -> from to where
  toInputObjectRowList :: SProxy path -> RLProxy iRl -> Builder (Record from) (Record to)

instance toInputObjectRowListNil ::
  ToInputObjectRowList path RowList.Nil () ()
  where
    toInputObjectRowList _ _ = identity
else instance toInputObjectRowListCons ::
  ( ToInputObjectType name path typ gType
  , ToInputObjectRowList path restRl from restTo
  -- Builder.insert
  , Row.Cons name (Record ( "type" :: GraphQLType gType)) restTo to
  , Row.Lacks name restTo
  , Symbol.IsSymbol name
  ) => ToInputObjectRowList path (RowList.Cons name typ restRl) from to
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

class ToInputObjectType (name :: Symbol) (path :: Symbol) typ gType | name path typ -> gType where
  toInputObjectType :: SProxy name -> SProxy path -> Proxy typ -> GraphQLType gType

instance toInputObjectFieldDispatch ::
  ( IsScalarPred typ isScalar
  , IsListPred typ isList
  , Row.IsRecordPred typ isRecord
  , ToInputObjectTypeDispatch isScalar isList isRecord name path typ gType
  ) => ToInputObjectType name path typ gType
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
  (name :: Symbol) (path :: Symbol) typ gType
  | isScalar isList isRecord name path typ -> gType
  where
    toInputObjectTypeDispatch
      :: BProxy isScalar -> BProxy isList -> BProxy isRecord
         -> SProxy name -> SProxy path -> Proxy typ -> GraphQLType gType

instance toInputObjectFieldIsScalar ::
  ( IsScalar typ
  ) => ToInputObjectTypeDispatch Bool.True isList isRecord name path typ typ
  where
    toInputObjectTypeDispatch _ _ _ _ _ _ = toScalar
else instance toInputObjectFieldIsList ::
  ( Symbol.Append name "-Item" name'
  , ToInputObjectType name' path a b
  , IsList f b
  ) => ToInputObjectTypeDispatch Bool.False Bool.True isRecord name path (f a) (f b)
  where
    toInputObjectTypeDispatch _ _ _ _ _ _
      = let
          item = toInputObjectType
                  (SProxy :: SProxy name')
                  (SProxy :: SProxy path)
                  (Proxy :: Proxy a)
        in
          toList item
else instance toInputObjectFieldIsRecord ::
  ( Symbol.Append path "_" path0
  , Symbol.Append path0 name path1
  , ToInputObjectWithPath path1 row o -- path1 row -> o -- NOTE o is not carried
  , Symbol.IsSymbol path1
  ) => ToInputObjectTypeDispatch Bool.False Bool.False Bool.True name path (Record row) (Record row)
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
else instance toInputObjectFieldIsNewType ::
  ( Generic typ (Constructor name1 (Argument (Record row)))
  , Symbol.Append path "_" path0
  , Symbol.Append path0 name path1
  , Symbol.Append path1 "-" path2
  , Symbol.Append path2 name1 path3
  , ToInputObjectWithPath path3 row o -- path1 row -> o -- NOTE o is not carried
  , Symbol.IsSymbol path3
  ) => ToInputObjectTypeDispatch Bool.False Bool.False Bool.False name path typ typ
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

