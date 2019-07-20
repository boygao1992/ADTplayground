module Data.Generic.Rep.Lens.Record where

import Prelude
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(..), RProxy, SProxy(..))

import Record.Builder (Builder)
import Record.Builder as Builder
import Data.Lens (Optic)
import Data.Lens.Record (prop)
import Data.Profunctor.Strong (class Strong)

class GenericLensRecord (i :: # Type) (o :: # Type) | i -> o where
  genericLensRecord :: RProxy i -> Record o

instance genericLensRecordImpl
  :: ( RowToList i iRl
    , GenericLensRL iRl o
    )
  => GenericLensRecord i o
  where
    genericLensRecord _
      = Builder.build <@> {}
      $ genericLensRL (RLProxy :: RLProxy iRl)

class GenericLensRL (iRl :: RowList) (to :: # Type) | iRl -> to where
  genericLensRL :: RLProxy iRl -> Builder {} (Record to)

instance genericLensRLNil :: GenericLensRL (RowList.Nil) () where
  genericLensRL _ = identity

instance genericLensRLCons
  :: ( IsSymbol label
    , GenericLensRL restRl restTo

    , Row.Cons label a r r1
    , Row.Cons label b r r2
    , Strong p
    , Row.Cons label (Optic p (Record r1) (Record r2) a b) restTo to

    , Row.Lacks label restTo
    )
  => GenericLensRL (RowList.Cons label typ restRl) to
  where
    genericLensRL _
      = Builder.insert
          (SProxy :: SProxy label)
          (prop (SProxy :: SProxy label))
      <<< genericLensRL (RLProxy :: RLProxy restRl)

