module Database.Beam.Backend.SQL.AST where

import Prelude hiding (Ordering)

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

newtype Select = Select
  { selectTable :: SelectTable
  , selectOrdering :: (Array Ordering)
  , selectLimit :: Maybe Int
  , selectOffset :: Maybe Int
  }


data SelectTable
  = SelectTable
      { selectQuantifier :: Maybe SetQuantifier
      , selectProjection :: Projection
      , selectFrom       :: Maybe From
      , selectWhere      :: Maybe Expression
      , selectGrouping   :: Maybe Grouping
      , selectHaving     :: Maybe Expression
      }
  | UnionTables Boolean SelectTable SelectTable
  | IntersectTables Boolean SelectTable SelectTable
  | ExceptTable Boolean SelectTable SelectTable


data SetQuantifier
  = SetQuantifierAll
  | SetQuantifierDistinct


newtype Projection
  = ProjExprs (Array (Tuple Expression (Maybe String)))


data Expression
  = ExpressionValue Value
  | ExpressionDefault
  | ExpressionRow ( Array Expression )

  | ExpressionIn Expression (Array Expression)

  | ExpressionIsNull Expression
  | ExpressionIsNotNull Expression
  | ExpressionIsTrue Expression
  | ExpressionIsNotTrue Expression
  | ExpressionIsFalse Expression
  | ExpressionIsNotFalse Expression
  | ExpressionIsUnknown Expression
  | ExpressionIsNotUnknown Expression

  | ExpressionCase (Array (Tuple Expression Expression)) Expression
  | ExpressionCoalesce (Array Expression)
  | ExpressionNullIf Expression Expression

  | ExpressionFieldName FieldName

  | ExpressionBetween Expression Expression Expression
  | ExpressionBinOp String Expression Expression
  | ExpressionCompOp String (Maybe ComparatorQuantifier) Expression Expression
  | ExpressionUnOp String Expression

  | ExpressionPosition Expression Expression
  | ExpressionCast Expression CastTarget
  | ExpressionExtract ExtractField Expression
  | ExpressionCharLength Expression
  | ExpressionOctetLength Expression
  | ExpressionBitLength Expression
  | ExpressionAbs Expression
  | ExpressionLower Expression
  | ExpressionUpper Expression
  | ExpressionTrim Expression

  | ExpressionFunctionCall Expression (Array Expression)
  | ExpressionInstanceField Expression String
  | ExpressionRefField Expression String

  | ExpressionCountAll
  | ExpressionAgg String (Maybe SetQuantifier) (Array Expression)
  | ExpressionBuiltinFunction String (Array Expression)

  | ExpressionSubquery Select
  | ExpressionUnique Select
  | ExpressionDistinct Select
  | ExpressionExists Select

  | ExpressionOver Expression WindowFrame

  | ExpressionCurrentTimestamp


-- data Value where
--   Value :: (Show a, Eq a, Typeable a) => a -> Value
data Value


data FieldName
  = QualifiedField String String
  | UnqualifiedField String


data ComparatorQuantifier
  = ComparatorQuantifierAny
  | ComparatorQuantifierAll

data CastTarget
  = CastTargetDataType DataType
  | CastTargetDomainName String


data DataType
  = DataTypeChar Boolean {- Varying -} (Maybe Int) (Maybe String)
  | DataTypeNationalChar Boolean (Maybe Int)
  | DataTypeBit Boolean (Maybe Int)
  | DataTypeNumeric (Maybe (Tuple Int (Maybe Int)))
  | DataTypeDecimal (Maybe (Tuple Int (Maybe Int)))
  | DataTypeInteger
  | DataTypeSmallInt
  | DataTypeBigInt
  | DataTypeFloat (Maybe Int)
  | DataTypeReal
  | DataTypeDoublePrecision
  | DataTypeDate
  | DataTypeTime (Maybe Int) {- time fractional seconds precision -} Boolean {- With time zone -}
  | DataTypeTimeStamp (Maybe Int) Boolean
  | DataTypeInterval ExtractField
  | DataTypeIntervalFromTo ExtractField ExtractField
  | DataTypeBooleanean

  | DataTypeBinaryLargeObject
  | DataTypeCharacterLargeObject

  | DataTypeArray DataType Int
  | DataTypeRow (Array (Tuple String DataType))

  | DataTypeDomain String


data ExtractField
  = ExtractFieldTimeZoneHour
  | ExtractFieldTimeZoneMinute

  | ExtractFieldDateTimeYear
  | ExtractFieldDateTimeMonth
  | ExtractFieldDateTimeDay
  | ExtractFieldDateTimeHour
  | ExtractFieldDateTimeMinute
  | ExtractFieldDateTimeSecond

data WindowFrame = WindowFrame
    { windowFramePartitions :: Maybe (Array Expression)
    , windowFrameOrdering   ::  Maybe (Array Ordering)
    , windowFrameBounds     :: Maybe WindowFrameBounds
    }


data WindowFrameBounds = WindowFrameBounds
    { boundsFrom :: WindowFrameBound
    , boundsTo   :: Maybe WindowFrameBound
    }

data WindowFrameBound
  = WindowFrameUnbounded
  | WindowFrameBoundNRows Int


data From
  = FromTable TableSource (Maybe String)
  | InnerJoin From From (Maybe Expression)
  | LeftJoin From From (Maybe Expression)
  | RightJoin From From (Maybe Expression)
  | OuterJoin From From (Maybe Expression)


data TableSource
  = TableNamed String
  | TableFromSubSelect Select

newtype Grouping = Grouping (Array Expression)
