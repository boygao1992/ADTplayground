module Type.Row.MkLabels where

import Prelude

import Type.Prelude (SProxy(..), RProxy, RLProxy(..))
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.Symbol as Symbol
import Prim.Row as Row

class MkLabels (i :: # Type) (o :: # Type) | i -> o where
  mkLabels :: RProxy i -> Record o

instance mkLabelsImpl ::
  ( RowList.RowToList i iRl
  , MkLabelsRowList iRl o
  ) => MkLabels i o
    where
      mkLabels _ = Builder.build (mkLabelsRowList (RLProxy :: RLProxy iRl)) {}


class MkLabelsRowList (iRl :: RowList) (to :: # Type) | iRl -> to where
  mkLabelsRowList :: RLProxy iRl -> Builder {} { | to}

instance mkLabelsRowListNil ::
  MkLabelsRowList RowList.Nil ()
    where
      mkLabelsRowList _ = identity
instance mkLabelsRowListCons ::
  ( MkLabelsRowList restIRl restTo
  , Symbol.IsSymbol label
  , Row.Lacks label restTo
  , Row.Cons label (SProxy label) restTo to
  ) => MkLabelsRowList (RowList.Cons label typ restIRl) to
    where
      mkLabelsRowList _ =
            Builder.insert
              (SProxy :: SProxy label)
              (SProxy :: SProxy label)
        <<< mkLabelsRowList
              (RLProxy :: RLProxy restIRl)
