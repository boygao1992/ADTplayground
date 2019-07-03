module Adhoc.Adapter where

import Prelude

import Adhoc.Cell (CellType(..))
import Data.Array as Array
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Fixed (class KnownPrecision, Fixed, PProxy(..), reflectPrecisionDecimalPlaces, toNumber)
import Data.List ((:), List(Nil), reverse)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Data.Symbol (class IsSymbol, SProxy, reflectSymbol)

newtype EnumType enum = EnumType enum
derive instance newtypeEnumType :: Newtype (EnumType enum) _

rowInputAdapter
  :: forall r
  . HFoldlWithIndex RowInputMapping
    (List (String /\ CellType))
    r
    (List (String /\ CellType))
  => r
  -> (List (String /\ CellType))
rowInputAdapter
  = reverse <<< hfoldlWithIndex RowInputMapping (Nil :: List (String /\ CellType))

data RowInputMapping = RowInputMapping

instance rowInputMappingReducerInt
  :: ( IsSymbol label
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) Int (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (CellTypeInt a)) : list

instance rowInputMappingReducerNumber
  :: ( IsSymbol label
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) Number (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (CellTypeNumber a)) : list

instance rowInputMappingReducerFixed
  :: ( IsSymbol label
    , KnownPrecision precision
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) (Fixed precision) (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ( (reflectSymbol label)
      /\ ( CellTypeFixed
          (reflectPrecisionDecimalPlaces (PProxy :: PProxy precision))
          (toNumber a)
        )
      ) : list

instance rowInputMappingReducerString
  :: ( IsSymbol label
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) String (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (CellTypeString a)) : list

instance rowInputMappingReducerEnum
  :: ( BoundedEnum enum
    , Show enum
    , IsSymbol label
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) (EnumType enum) (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list (EnumType enum) =
      let
        (available :: Array enum)
          = Array.mapMaybe toEnum
          $ Array.range (fromEnum (bottom :: enum)) (fromEnum (top :: enum))
      in
        ( (reflectSymbol label)
        /\ (CellTypeEnum (show <$> available) (show enum))
        ) : list
