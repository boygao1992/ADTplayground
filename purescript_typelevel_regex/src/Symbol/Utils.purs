module Symbol.Utils where

import Prelude

import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

-- | Remove Space
class RemoveSpace (i :: Symbol) (o :: Symbol) | i -> o

instance removeSpaceEmpty :: RemoveSpace "" ""
else instance removeSpaceAll ::
  ( Symbol.Cons h t i -- h t <- i
  , RemoveSpaceImpl h t o -- h t -> o
  ) => RemoveSpace i o

class RemoveSpaceImpl (h :: Symbol) (t :: Symbol) (o :: Symbol) | h t ->o

instance removeSpaceImplInductionStepSpace ::
  ( RemoveSpace t o -- t -> o
  ) => RemoveSpaceImpl " " t o
else instance removeSpaceImplInductionStepNonSpace ::
  ( RemoveSpace t rest -- t -> rest
  , Symbol.Append h rest o -- h rest -> o
  ) => RemoveSpaceImpl h t o

removeSpace :: forall i o. RemoveSpace i o => SProxy i -> SProxy o
removeSpace _ = SProxy :: SProxy o

-- | ReverseSymbol

class ReverseSymbol (i :: Symbol) (o :: Symbol) | i -> o

instance reverseSymbolEmpty :: ReverseSymbol "" ""
else instance reverseSymbolOtherwise ::
  (Symbol.Cons h t i -- h t <- i
  , ReverseSymbolImpl h t o -- h t -> o
  ) => ReverseSymbol i o

class ReverseSymbolImpl (h :: Symbol) (t :: Symbol) (o :: Symbol) | h t -> o

instance reverseSymbolImplInductionStep ::
  ( ReverseSymbol t rest -- t -> rest
  , Symbol.Append rest h o -- rest h -> o
  ) => ReverseSymbolImpl h t o

reverseSymbol :: forall i o. ReverseSymbol i o => SProxy i -> SProxy o
reverseSymbol _ = SProxy :: SProxy o
