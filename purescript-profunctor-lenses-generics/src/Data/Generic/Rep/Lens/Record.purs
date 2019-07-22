module Data.Generic.Rep.Lens.Record where

import Prelude

import Data.Lens (Optic')
import Data.Lens.Record (prop)
import Data.Profunctor.Strong (class Strong)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(..), RProxy(..), SProxy(..))

class GenericLensRecord p s (i :: # Type) (o :: # Type) | i -> o where
  genericLensRecord :: RProxy i -> Optic' p s (Record i) -> Record o

instance genericLensRecordImpl
  :: ( RowToList i iRl
    , GenericLensRL p s i iRl o
    )
  => GenericLensRecord p s i o
  where
    genericLensRecord _ _i
      = Builder.build <@> {}
      $ genericLensRL (RProxy :: RProxy i) (RLProxy :: RLProxy iRl) _i

class GenericLensRL p s i (iRl :: RowList) (to :: # Type) | p s i iRl -> to where
  genericLensRL :: RProxy i -> RLProxy iRl -> Optic' p s (Record i) -> Builder {} (Record to)

instance genericLensRLNil :: GenericLensRL p s i (RowList.Nil) () where
  genericLensRL _ _ _ = identity

instance genericLensRLCons
  :: ( IsSymbol label
    , Row.Cons label typ r i
    , Strong p
    , Row.Cons label (Optic' p s typ) restTo to
    , Row.Lacks label restTo
    , GenericLensRL p s i restRl restTo
    )
  => GenericLensRL p s i (RowList.Cons label typ restRl) to
  where
    genericLensRL _ _ _i
      = Builder.insert
          (SProxy :: SProxy label)
          (_i <<< prop (SProxy :: SProxy label))
      <<< genericLensRL (RProxy :: RProxy i) (RLProxy :: RLProxy restRl) _i
