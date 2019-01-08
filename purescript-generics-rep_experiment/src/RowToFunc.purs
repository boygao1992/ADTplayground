module RowToFunc where

import Prelude

import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Row (RProxy(..))
import Type.Row as Row

class RowToConstructor (row :: # Type) func | row -> func
  where
    rowToCons :: RProxy row -> func

instance rowToConstructorImpl ::
  ( RowList.RowToList row rl
  , RowListToConstructorInit rl func
  ) => RowToConstructor row func
  where
    rowToCons _ = listToConsInit (RLProxy :: RLProxy rl)

class RowListToConstructorInit (rl :: RowList) func | rl -> func where
  listToConsInit :: RLProxy rl -> func

instance rowListToConstructorInit ::
  ( RowListToConstructor () rl func
  ) => RowListToConstructorInit rl func where
  listToConsInit rlp = listToCons (RProxy :: RProxy ()) rlp identity

class RowListToConstructor (r :: # Type) (rl :: RowList) func | r rl -> func
  where
    listToCons :: RProxy r -> RLProxy rl -> Builder (Record ()) (Record r) -> func

instance rowListToConstructorBaseCase ::
  ( Row.Cons name typ r r'
  , Symbol.IsSymbol name
  , Row.Lacks name r
  ) => RowListToConstructor r (RowList.Cons name typ RowList.Nil) (typ -> { | r' })
  where
    listToCons _ _ builder = \x -> Builder.build (Builder.insert (SProxy :: SProxy name) x <<< builder) {}
else instance rowListToConstructorInductionStep ::
  ( Row.Cons name typ r r'
  , RowListToConstructor r' restRl restFunc
  , Symbol.IsSymbol name
  , Row.Lacks name r
  ) => RowListToConstructor r (RowList.Cons name typ restRl) (typ -> restFunc)
  where
    listToCons _ _ builder = \x -> listToCons (RProxy :: RProxy r') (RLProxy :: RLProxy restRl) (Builder.insert (SProxy :: SProxy name) x <<< builder)

