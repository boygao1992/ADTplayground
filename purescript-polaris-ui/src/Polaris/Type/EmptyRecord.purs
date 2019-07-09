module Polaris.Type.EmptyRecord where

import Prelude

import Data.Maybe (Maybe(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (class IsSymbol, SProxy(..))

class EmptyRecord (i :: # Type) (o :: # Type) | i -> o where
  emptyRecord ::  RProxy i -> Record o

instance emptyRecordBuild
  :: ( RL.RowToList i iRl
    , EmptyRecordRL iRl o
    )
  => EmptyRecord i o where
    emptyRecord _ = (Builder.build <@> {}) (emptyRecordRL (RLProxy :: RLProxy iRl))


class EmptyRecordRL (iRl :: RL.RowList) (o :: # Type) | iRl -> o where
  emptyRecordRL :: RLProxy iRl -> Builder.Builder {} {|o}

instance emptyRecordRLNil :: EmptyRecordRL RL.Nil () where
  emptyRecordRL _ = identity
instance emptyRecordRLCons
  :: ( IsSymbol label
    , Row.Lacks label restO
    , Row.Cons label (Maybe typ) restO o
    , EmptyRecordRL restRl restO
    )
  => EmptyRecordRL (RL.Cons label (Maybe typ) restRl) o where
  emptyRecordRL _
    = Builder.insert
      (SProxy :: SProxy label)
      Nothing
      <<< emptyRecordRL (RLProxy :: RLProxy restRl)
