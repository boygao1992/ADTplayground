module Data.Lens.Record.MkLenses where

import Prelude

import Data.Lens (Optic)
import Data.Lens.Record (prop)
import Data.Profunctor.Strong (class Strong)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..))

type RLenses (i :: # Type) = forall o. MkLenses i o => Record o

class MkLenses (i :: # Type) (o :: # Type) | i -> o where
  mkLenses :: forall f. f i -> Record o

instance mkLensesInit
  :: ( RL.RowToList i rl
    , MkLensesImpl rl o
    )
  => MkLenses i o
  where
    mkLenses _ = (Builder.build <@> {}) (mkLensesImpl (RLProxy :: RLProxy rl))

class MkLensesImpl (rl :: RL.RowList) (to :: # Type) | rl -> to where
  mkLensesImpl :: RLProxy rl -> Builder (Record ()) (Record to)

instance mkLensesImplNil :: MkLensesImpl RL.Nil () where
  mkLensesImpl _ = identity

instance mkLensesImplCons
  :: ( MkLensesImpl restRl restTo
    , IsSymbol label
    , Row.Lacks label restTo
    , Row.Cons label a r r1
    , Row.Cons label b r r2
    , Strong p
    , Row.Cons label (Optic p ({ | r1 }) ({ | r2 }) a b) restTo to
    )
  => MkLensesImpl (RL.Cons label typ restRl) to
  where
    mkLensesImpl _ =
      Builder.insert
        (SProxy :: SProxy label)
        (prop (SProxy :: SProxy label))
      <<< (mkLensesImpl (RLProxy :: RLProxy restRl))
