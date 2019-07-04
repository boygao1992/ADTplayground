module Adhoc.Adapter where

import Prelude

import Adhoc.Cell (CellType(..))
import Data.Array as Array
import Data.Either as Either
import Data.Fixed (class KnownPrecision, Fixed, PProxy(..), reflectPrecisionDecimalPlaces)
import Data.Fixed as Fixed
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.EnumReadSymbol (class EnumReadSymbol, enumReadSymbol)
import Data.Generic.Rep.EnumToList (class GenericEnumToList, enumToList)
import Data.Generic.Rep.Show (class GenericShow, genericShow)
import Data.Lens.Index.Recordable (class RecordableWithRelation, class Relation, toRecordWithRelation)
import Data.List (List(Nil), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class Mapping, mapping)
import Type.Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Type.Proxy (Proxy(..))

-------
-- Data

-- newtype EnumType enum = EnumType enum
-- derive instance newtypeEnumType :: Newtype (EnumType enum) _

---------------
-- FromCellType :: CellType -> Maybe a

data FromCellType = FromCellType

instance fromCellTypeInt :: Relation FromCellType CellType (Maybe Int) where
  relation _ (CellTypeInt int) = Just int
  relation _ _ = Nothing
else
instance fromCellTypeNumber :: Relation FromCellType CellType (Maybe Number) where
  relation _ (CellTypeNumber num) = Just num
  relation _ _ = Nothing
else
instance fromCellTypeFixed :: KnownPrecision precision => Relation FromCellType CellType (Maybe (Fixed precision)) where
  relation _ (CellTypeFixed _ num) = Fixed.fromNumber num
  relation _ _ = Nothing
else
instance fromCellTypeString :: Relation FromCellType CellType (Maybe String) where
  relation _ (CellTypeString str) = Just str
  relation _ _ = Nothing
else
instance fromCellTypeEnum
  :: ( Generic enum enumRep
    , EnumReadSymbol enumRep
    )
  => Relation FromCellType CellType (Maybe enum) where
  relation _ (CellTypeEnum _ choice) = Either.hush $ enumReadSymbol choice
  relation _ _ = Nothing

-------------
-- ToCellType :: a -> CellType

data ToCellType = ToCellType

instance toCellTypeInt :: Mapping ToCellType Int CellType where
  mapping ToCellType int = CellTypeInt int
else
instance toCellTypeNumber :: Mapping ToCellType Number CellType where
  mapping ToCellType number = CellTypeNumber number
else
instance toCellTypeFixed
  :: KnownPrecision precision
  => Mapping ToCellType (Fixed precision) CellType
  where
  mapping ToCellType fixed
    = CellTypeFixed
      (reflectPrecisionDecimalPlaces (PProxy :: PProxy precision))
      (Fixed.toNumber fixed)
else
instance toCellTypeString :: Mapping ToCellType String CellType where
  mapping ToCellType str = CellTypeString str
else
instance toCellTypeEnum
  :: ( Generic enum enumRep
    , GenericEnumToList enumRep
    , GenericShow enumRep
    , Show enum
    )
  => Mapping ToCellType enum CellType where
  mapping ToCellType enum =
    CellTypeEnum (Array.fromFoldable $ enumToList (Proxy :: Proxy enum)) (genericShow enum)

------------------
-- rowInputAdapter :: Record r -> List (String /\ CellType)

rowInputAdapter
  :: forall r
  . HFoldlWithIndex RowInputMapping
    (List (String /\ CellType))
    r
    (List (String /\ CellType))
  => r
  -> (Array (String /\ CellType))
rowInputAdapter
  = Array.fromFoldable <<< hfoldlWithIndex RowInputMapping (Nil :: List (String /\ CellType))

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
else
instance rowInputMappingReducerNumber
  :: ( IsSymbol label
    , Mapping ToCellType Number CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) Number (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list
else
instance rowInputMappingReducerFixed
  :: ( IsSymbol label
    , Mapping ToCellType (Fixed precision) CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) (Fixed precision) (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list
else
instance rowInputMappingReducerString
  :: ( IsSymbol label
    , Mapping ToCellType String CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) String (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list
else
instance rowInputMappingReducerEnum
  :: ( IsSymbol label
    , Mapping ToCellType enum CellType
    )
  => FoldingWithIndex RowInputMapping
    (SProxy label) (List (String /\ CellType)) enum (List (String /\ CellType))
  where
    foldingWithIndex RowInputMapping label list a =
      ((reflectSymbol label) /\ (mapping ToCellType a)) : list

--------------------
-- tableInputAdapter :: Array (Record r) -> List (List (String /\ CellType))
tableInputAdapter
  :: forall r
  . HFoldlWithIndex RowInputMapping
     (List (String /\ CellType))
     r
     (List (String /\ CellType))
  => Array r
  -> Array (Array (String /\ CellType))
tableInputAdapter = map rowInputAdapter

-------------------
-- rowOutputAdapter :: Map String CellType -> Maybe (Record r)

rowOutputAdapter
  :: forall r
  . RecordableWithRelation FromCellType (Map String CellType) r
  => Map String CellType
  -> Maybe (Record r)
rowOutputAdapter = toRecordWithRelation FromCellType

---------------------
-- tableOutputAdapter :: Map Int (Map String CellType) -> Maybe (Array (Record r))

tableOutputAdapter
  :: forall r
  . RecordableWithRelation FromCellType (Map String CellType) r
  => Map Int (Map String CellType)
  -> Maybe (Array (Record r))
tableOutputAdapter = traverse rowOutputAdapter <<< Array.fromFoldable

------------------- Text

data TestEnum
  = A
  | B
  | C
derive instance genericTestEnum :: Generic TestEnum _
instance showTestEnum :: Show TestEnum where show = genericShow

sampleInput ::
  { id :: Int
  , name :: String
  , enum :: TestEnum
  , bias :: Number
  }
sampleInput = { id: 0, name: "wenbo", enum: B, bias: 14.0 }

testInput :: Array (String /\ CellType)
testInput = rowInputAdapter sampleInput

sampleOutput :: Map String CellType
sampleOutput
  = Map.insert "id" (CellTypeInt 0)
  $ Map.insert "name" (CellTypeString "wenbo")
  $ Map.insert "enum" (CellTypeEnum ["A", "B", "C"] "B")
  $ Map.insert "bias" (CellTypeNumber 14.0)
  $ Map.insert "value" (CellTypeFixed 3 123.4567)
  $ Map.empty

testOutput :: Maybe { id :: Int, name :: String, enum :: TestEnum, bias :: Number, value :: Fixed Fixed.P1000 }
testOutput = rowOutputAdapter sampleOutput
