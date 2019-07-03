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
import Heterogeneous.Mapping (class Mapping, mapping)
import Type.Data.Symbol (class IsSymbol, SProxy, reflectSymbol)

newtype EnumType enum = EnumType enum
derive instance newtypeEnumType :: Newtype (EnumType enum) _

data ToCellType = ToCellType

instance toCellTypeInt :: Mapping ToCellType Int CellType where
  mapping ToCellType int = CellTypeInt int
instance toCellTypeNumber :: Mapping ToCellType Number CellType where
  mapping ToCellType number = CellTypeNumber number
instance toCellTypeFixed
  :: KnownPrecision precision
  => Mapping ToCellType (Fixed precision) CellType
  where
  mapping ToCellType fixed
    = CellTypeFixed
      (reflectPrecisionDecimalPlaces (PProxy :: PProxy precision))
      (toNumber fixed)
instance toCellTypeString :: Mapping ToCellType String CellType where
  mapping ToCellType str = CellTypeString str
instance toCellTypeEnum
  :: ( BoundedEnum enum
    , Show enum
    )
  => Mapping ToCellType (EnumType enum) CellType where
  mapping ToCellType (EnumType enum) =
    let
      (available :: Array enum)
        = Array.mapMaybe toEnum
        $ Array.range (fromEnum (bottom :: enum)) (fromEnum (top :: enum))
    in
      CellTypeEnum (show <$> available) (show enum)

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
    , Mapping ToCellType Int CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) Int (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list

instance rowInputMappingReducerNumber
  :: ( IsSymbol label
    , Mapping ToCellType Number CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) Number (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list

instance rowInputMappingReducerFixed
  :: ( IsSymbol label
    , KnownPrecision precision
    , Mapping ToCellType (Fixed precision) CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) (Fixed precision) (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list

instance rowInputMappingReducerString
  :: ( IsSymbol label
    , Mapping ToCellType String CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) String (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list

instance rowInputMappingReducerEnum
  :: ( BoundedEnum enum
    , Show enum
    , IsSymbol label
    , Mapping ToCellType (EnumType enum) CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) (EnumType enum) (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list
