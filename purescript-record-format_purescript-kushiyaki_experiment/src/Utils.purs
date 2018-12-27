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

instance removeSpaceImplBaseCase1 :: RemoveSpaceImpl " " "" ""
else instance removeSpaceImplBaseCase2 :: RemoveSpaceImpl h "" h
else instance removeSpaceImplInductionStepSpace ::
  ( Symbol.Cons t_h t_t t
  , RemoveSpaceImpl t_h t_t o
  ) => RemoveSpaceImpl " " t o
else instance removeSpaceImplInductionStepNonSpace ::
  ( Symbol.Cons t_h t_t t
  , RemoveSpaceImpl t_h t_t rest
  , Symbol.Append h rest o
  ) => RemoveSpaceImpl h t o

removeSpace :: forall i o. RemoveSpace i o => SProxy i -> SProxy o
removeSpace _ = SProxy :: SProxy o
