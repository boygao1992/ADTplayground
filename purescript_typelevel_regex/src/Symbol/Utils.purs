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

-- | switch case the first character

class SwitchCaseFirstChar (upper :: Symbol) (lower :: Symbol) | upper -> lower, lower -> upper

instance lowerFirstCharEmpty :: SwitchCaseFirstChar "" ""
else instance lowerFirstCharOtherwise ::
  ( Symbol.Cons h_upper t upper -- h_upper t <-> upper
  , SwitchCase h_upper h_lower -- h_upper <-> h_lower
  , Symbol.Cons h_lower t lower -- h_lower t <-> lower
  ) => SwitchCaseFirstChar upper lower

-- Test
-- lowerFirstChar :: forall i o. SwitchCaseFirstChar i o => SProxy i -> SProxy o
-- lowerFirstChar _ = SProxy :: SProxy o
-- lowerFirstCharExample :: SProxy "createNewRecord"
-- lowerFirstCharExample = lowerFirstChar (SProxy :: SProxy "CreateNewRecord")
-- uppercaseFirstChar :: forall i o. SwitchCaseFirstChar o i => SProxy i -> SProxy o
-- uppercaseFirstChar _ = SProxy :: SProxy o
-- uppercaseFirstCharExample :: SProxy "CreateNewRecord"
-- uppercaseFirstCharExample = uppercaseFirstChar (SProxy :: SProxy "createNewRecord")

-- | SwitchCase

class SwitchCase (upper :: Symbol) (lower :: Symbol) | upper -> lower, lower -> upper

instance switchCaseA :: SwitchCase "A" "a"
instance switchCaseB :: SwitchCase "B" "b"
instance switchCaseC :: SwitchCase "C" "c"
instance switchCaseD :: SwitchCase "D" "d"
instance switchCaseE :: SwitchCase "E" "e"
instance switchCaseF :: SwitchCase "F" "f"
instance switchCaseG :: SwitchCase "G" "g"
instance switchCaseH :: SwitchCase "H" "h"
instance switchCaseI :: SwitchCase "I" "i"
instance switchCaseJ :: SwitchCase "J" "j"
instance switchCaseK :: SwitchCase "K" "k"
instance switchCaseL :: SwitchCase "L" "l"
instance switchCaseM :: SwitchCase "M" "m"
instance switchCaseN :: SwitchCase "N" "n"
instance switchCaseO :: SwitchCase "O" "o"
instance switchCaseP :: SwitchCase "P" "p"
instance switchCaseQ :: SwitchCase "Q" "q"
instance switchCaseR :: SwitchCase "R" "r"
instance switchCaseS :: SwitchCase "S" "s"
instance switchCaseT :: SwitchCase "T" "t"
instance switchCaseU :: SwitchCase "U" "u"
instance switchCaseV :: SwitchCase "V" "v"
instance switchCaseW :: SwitchCase "W" "w"
instance switchCaseX :: SwitchCase "X" "x"
instance switchCaseY :: SwitchCase "Y" "y"
instance switchCaseZ :: SwitchCase "Z" "z"
