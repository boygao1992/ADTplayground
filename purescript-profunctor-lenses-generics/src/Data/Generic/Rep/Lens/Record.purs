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

class GenericLensRecord (i :: # Type) (o :: # Type) | i -> o where
  genericLensRecord :: RProxy i -> Record o

instance genericLensRecordImpl
  :: ( RowToList i iRl
    , GenericLensRL i iRl o
    )
  => GenericLensRecord i o
  where
    genericLensRecord _
      = Builder.build <@> {}
      $ genericLensRL (RProxy :: RProxy i) (RLProxy :: RLProxy iRl)

class GenericLensRL i (iRl :: RowList) (to :: # Type) | iRl -> to where
  genericLensRL :: RProxy i -> RLProxy iRl -> Builder {} (Record to)

instance genericLensRLNil :: GenericLensRL i (RowList.Nil) () where
  genericLensRL _ _ = identity

instance genericLensRLCons
  :: ( IsSymbol label
    , Row.Cons label typ r i
    , Strong p
    , Row.Cons label (Optic' p (Record i) typ) restTo to
    , Row.Lacks label restTo
    , GenericLensRL i restRl restTo
    )
  => GenericLensRL i (RowList.Cons label typ restRl) to
  where
    genericLensRL _ _
      = Builder.insert
          (SProxy :: SProxy label)
          (prop (SProxy :: SProxy label))
      <<< genericLensRL (RProxy :: RProxy i) (RLProxy :: RLProxy restRl)
