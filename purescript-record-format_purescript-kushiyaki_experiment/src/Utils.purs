module Utils where

import Prelude

import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

-- | Remove Space
class RemoveSpace (i :: Symbol) (o :: Symbol) | i -> o

instance removeSpaceEmpty :: RemoveSpace "" ""
else instance removeSpaceAll ::
  ( Symbol.Cons h t i
  , RemoveSpaceImpl h t o
  ) => RemoveSpace i o

class RemoveSpaceImpl (h :: Symbol) (t :: Symbol) (o :: Symbol) | h t ->o

instance removeSpaceImplInductionStepSpace ::
  ( RemoveSpace t o
  ) => RemoveSpaceImpl " " t o
else instance removeSpaceImplInductionStepNonSpace ::
  ( RemoveSpace t rest
  , Symbol.Append h rest o
  ) => RemoveSpaceImpl h t o

removeSpace :: forall i o. RemoveSpace i o => SProxy i -> SProxy o
removeSpace _ = SProxy :: SProxy o
