module Deprecated.FuncArg where

import Type.Row.Validation

import Prim.Row (class Cons) as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Type.Data.Boolean as Bool
import Type.Row.Utils (class HasFieldPred) as Row


class ToFuncArg (schema :: # Type) i (arg :: # Type) | schema i -> arg

instance toFuncArgIsRecord ::
  ( ToRowFuncArg schema iRow arg
  ) => ToFuncArg schema (Record iRow) arg
else instance toFuncArgNotRecord ::
  ToFuncArg schema i ()

class ToRowFuncArg (schema :: # Type) (i :: # Type) (arg :: # Type) | schema i -> arg

instance toRowFuncArgToRowList ::
  ( RowList.RowToList schema schemaRl
  , ToRowFuncArgRowList schemaRl i arg
  ) => ToRowFuncArg schema i arg

class ToRowFuncArgRowList (schemaRl :: RowList) (i :: # Type) (arg :: # Type) | schemaRl i  -> arg

instance toRowFuncArgRowListNil ::
  ToRowFuncArgRowList RowList.Nil i ()
else instance toRowFuncArgRowListConsOptional ::
  ( ToRowFuncArgRowList restRl i restArg
  , Row.HasFieldPred i name hasField
  , ToRowFuncArgRowListOptional hasField name typ restArg arg
  ) => ToRowFuncArgRowList (RowList.Cons name (Optional typ) restRl) i arg
else instance toRowFuncArgRowListConsRequired ::
  ( ToRowFuncArgRowList restRl i restArg
  , Row.HasFieldPred i name hasField
  , ToRowFuncArgRowListRequired hasField name typ restArg arg
  ) => ToRowFuncArgRowList (RowList.Cons name (Required typ) restRl) i arg
else instance toRowFuncArgRowListConsInvalidPattern ::
  Fail
  ( Above
    ( Beside
      ( Text "Invalid pattern for field `")
      ( Beside (Text name) (Text "`:"))
    )
    ( Quote pattern)
  )
  => ToRowFuncArgRowList (RowList.Cons name other restRl) i arg

class ToRowFuncArgRowListOptional (hasField :: Bool.Boolean) (name :: Symbol) typ (restArg :: # Type) (arg :: # Type)
  | hasField name typ restArg -> arg

instance toRowFuncArgRowListOptionalHasField ::
  ( Row.Cons name typ restArg arg
  ) => ToRowFuncArgRowListOptional Bool.True name typ restArg arg
else instance toRowFuncArgRowListOptionalNoField ::
  ToRowFuncArgRowListOptional Bool.False name typ restArg restArg

class ToRowFuncArgRowListRequired (hasField :: Bool.Boolean) (name :: Symbol) typ (restArg :: # Type) (arg :: # Type)
  | hasField name typ restArg -> arg

instance toRowFuncArgRowListRequiredHasField ::
  ( Row.Cons name typ restArg arg
  ) => ToRowFuncArgRowListRequired Bool.True name typ restArg arg
else instance toRowFuncArgRowListRequiredNoField ::
  Fail
  (Beside
   (Text "Required field `") (Beside (Text name) (Text "` is not provided."))
  )
  => ToRowFuncArgRowListRequired Bool.False name typ restArg arg

-- Test
-- toFuncArg
--   :: forall schema i arg o
--    . ToFuncArg schema i arg
--   => Type.IsEqual i (Record arg)
--   => RProxy schema
--   -> (i -> o)
--   -> RProxy arg
-- toFuncArg _ _ = RProxy :: RProxy arg

-- toFuncArgExample
--   = toFuncArg
--     (RProxy :: RProxy
--                ( id :: Required String
--                , age :: Optional Int
--                )
--     )
--     (\({id, age}) -> true)

-- NOTE HasFieldPred relies on RowToList which only works on closed Records
-- , but Records in function arguments are open
-- , thus the shape of input Records has to be rigid
-- TODO use Maybe

