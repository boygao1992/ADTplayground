module Beam.Backend.SQL.Types where

import Prelude hiding (Ordering(..))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Exists1 (Exists)

type Bool = Boolean
type Word = Int
type Text = String

data Command
  = SelectCommand Select
  | InsertCommand Insert
  | UpdateCommand Update
  | DeleteCommand Delete
derive instance genericCommand :: Generic Command _
derive instance eqCommand :: Eq Command
instance showCommand :: Show Command where show x = genericShow x

-- TODO instance IsSql92Syntax Command where

newtype Select = Select
  { table :: SelectTable
  , ordering :: Array Ordering
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
derive instance genericSelect :: Generic Select _
derive instance eqSelect :: Eq Select
instance showSelect :: Show Select where show x = genericShow x

-- TODO instance IsSql92SelectSyntax Select where

data SelectTable
  = SelectTable
    { quantifier :: Maybe SetQuantifier
    , projection :: Projection
    , from :: From
    , "where" :: Maybe Expression
    , grouping :: Maybe Grouping
    , having :: Maybe Expression
    }
  | UnionTables Bool SelectTable SelectTable
  | IntersectTables Bool SelectTable SelectTable
  | ExceptTable Bool SelectTable SelectTable
derive instance genericSelectTable :: Generic SelectTable _
derive instance eqSelectTable :: Eq SelectTable
instance showSelectTable :: Show SelectTable where show x = genericShow x

-- TODO instance IsSql92SelectTableSyntax SelectTable where

newtype Insert = Insert
  { table :: TableName
  , fields :: Array String
  , values :: InsertValues
  }
derive instance genericInsert :: Generic Insert _
derive instance eqInsert :: Eq Insert
instance showInsert :: Show Insert where show x = genericShow x

-- TODO instance IsSql92InsertSyntax Insert where

data InsertValues
  = InsertValues (Array (Array Expression)) -- NOTE insertValuesExpressions
  | InsertSelect Select -- NOTE insertSelectStmt
derive instance genericInsertValues :: Generic InsertValues _
derive instance eqInsertValues :: Eq InsertValues
instance showInsertValues :: Show InsertValues where show x = genericShow x

-- instance IsSql92InsertValuesSyntax InsertValues where

newtype Update = Update
  { table :: TableName
  , fields :: Array (FieldName /\ Expression)
  , "where" :: Maybe Expression
  }
derive instance genericUpdate :: Generic Update _
derive instance eqUpdate :: Eq Update
instance showUpdate :: Show Update where show x = genericShow x

-- TODO instance IsSql92UpdateSyntax Update where

newtype Delete = Delete
  { table :: TableName
  , alias :: Maybe String
  , "where" :: Maybe Expression
  }
derive instance genericDelete :: Generic Delete _
derive instance eqDelete :: Eq Delete
instance showDelete :: Show Delete where show x = genericShow x

-- TODO instance IsSql92DeleteSyntax Delete where

data FieldName
  = QualifiedField String String
  | UnqualifiedField String
derive instance genericFieldName :: Generic FieldName _
derive instance eqFieldName :: Eq FieldName
instance showFieldName :: Show FieldName where show x = genericShow x

-- TODO instance IsSql92FieldNameSyntax FieldName where

data ComparatorQuantifier
  = ComparatorQuantifierAny
  | ComparatorQuantifierAll
derive instance genericComparatorQuantifier :: Generic ComparatorQuantifier _
derive instance eqComparatorQuantifier :: Eq ComparatorQuantifier
instance showComparatorQuantifier :: Show ComparatorQuantifier where show x = genericShow x

-- TODO instance IsSql92QuantifierSyntax ComparatorQuantifier where

data ExtractField
  = ExtractFieldTimeZoneHour
  | ExtractFieldTimeZoneMinute

  | ExtractFieldDateTimeYear
  | ExtractFieldDateTimeMonth
  | ExtractFieldDateTimeDay
  | ExtractFieldDateTimeHour
  | ExtractFieldDateTimeMinute
  | ExtractFieldDateTimeSecond
derive instance genericExtractField :: Generic ExtractField _
derive instance eqExtractField :: Eq ExtractField
instance showExtractField :: Show ExtractField where show x = genericShow x

data DataType
  = DataTypeChar Bool {- Varying -} (Maybe Word) (Maybe Text)
  | DataTypeNationalChar Bool (Maybe Word)
  | DataTypeBit Bool (Maybe Word)
  | DataTypeNumeric (Maybe (Word /\ Maybe Word))
  | DataTypeDecimal (Maybe (Word /\ Maybe Word))
  | DataTypeInteger
  | DataTypeSmallInt
  | DataTypeBigInt
  | DataTypeFloat (Maybe Word)
  | DataTypeReal
  | DataTypeDoublePrecision
  | DataTypeDate
  | DataTypeTime (Maybe Word) {- time fractional seconds precision -} Bool {- With time zone -}
  | DataTypeTimeStamp (Maybe Word) Bool
  | DataTypeInterval ExtractField
  | DataTypeIntervalFromTo ExtractField ExtractField
  | DataTypeBoolean

  | DataTypeBinaryLargeObject
  | DataTypeCharacterLargeObject

  | DataTypeArray DataType Int
  | DataTypeRow (Array (Text /\ DataType))

  | DataTypeDomain Text
derive instance genericDataType :: Generic DataType _
derive instance eqDataType :: Eq DataType
instance showDataType :: Show DataType where show x = genericShow x

-- TODO instance IsSql92DataTypeSyntax DataType where
-- TODO instance IsSql99DataTypeSyntax DataType where
-- TODO instance IsSql2008BigIntDataTypeSyntax DataType where

data SetQuantifier
  = SetQuantifierAll
  | SetQuantifierDistinct
derive instance genericSetQuantifier :: Generic SetQuantifier _
derive instance eqSetQuantifier :: Eq SetQuantifier
instance showSetQuantifier :: Show SetQuantifier where show x = genericShow x

-- TODO instance IsSql92AggregationSetQuantifierSyntax SetQuantifier where

data Expression
  = ExpressionValue Value
  | ExpressionDefault
  | ExpressionRow (Array Expression)

  | ExpressionIn Expression (Array Expression)

  | ExpressionIsNull Expression
  | ExpressionIsNotNull Expression
  | ExpressionIsTrue Expression
  | ExpressionIsNotTrue Expression
  | ExpressionIsFalse Expression
  | ExpressionIsNotFalse Expression
  | ExpressionIsUnknown Expression
  | ExpressionIsNotUnknown Expression

  | ExpressionCase (Array (Expression /\ Expression)) Expression
  | ExpressionCoalesce (Array Expression)
  | ExpressionNullIf Expression Expression

  | ExpressionFieldName FieldName

  | ExpressionBetween Expression Expression Expression
  | ExpressionBinOp Text Expression Expression
  | ExpressionCompOp Text (Maybe ComparatorQuantifier) Expression Expression
  | ExpressionUnOp Text Expression

  | ExpressionPosition Expression Expression
  | ExpressionCast Expression DataType
  | ExpressionExtract ExtractField Expression
  | ExpressionCharLength Expression
  | ExpressionOctetLength Expression
  | ExpressionBitLength Expression
  | ExpressionAbs Expression
  | ExpressionLower Expression
  | ExpressionUpper Expression
  | ExpressionTrim Expression

  | ExpressionNamedFunction Text
  | ExpressionFunctionCall Expression (Array Expression)
  | ExpressionInstanceField Expression Text
  | ExpressionRefField Expression Text

  | ExpressionCountAll
  | ExpressionAgg Text (Maybe SetQuantifier) (Array Expression)
  | ExpressionBuiltinFunction Text (Array Expression)

  | ExpressionSubquery Select
  | ExpressionUnique Select
  | ExpressionDistinct Select
  | ExpressionExists Select

  | ExpressionOver Expression WindowFrame

  | ExpressionCurrentTimestamp
derive instance genericExpression :: Generic Expression _
derive instance eqExpression :: Eq Expression
instance showExpression :: Show Expression where show x = genericShow x

-- TODO instance IsSql92ExtractFieldSyntax ExtractField where
-- TODO instance IsSql92ExpressionSyntax Expression where
-- TODO instance IsSql99FunctionExpressionSyntax Expression where
-- TODO instance IsSql99ExpressionSyntax Expression where
-- TODO instance IsSql92AggregationExpressionSyntax Expression where
-- TODO instance IsSql99AggregationExpressionSyntax Expression where
-- TODO instance IsSql2003EnhancedNumericFunctionsExpressionSyntax Expression where
-- TODO instance IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax Expression where
-- TODO instance IsSql2003NtileExpressionSyntax Expression where
-- TODO instance IsSql2003LeadAndLagExpressionSyntax Expression where
-- TODO instance IsSql2003NthValueExpressionSyntax Expression where
-- TODO instance IsSql2003ExpressionSyntax Expression where

newtype Projection = ProjExprs (Array (Expression /\ Maybe Text))
derive instance genericProjection :: Generic Projection _
derive instance eqProjection :: Eq Projection
instance showProjection :: Show Projection where show x = genericShow x

-- TODO instance IsSql92ProjectionSyntax Projection where

data Ordering
  = OrderingAsc Expression
  | OrderingDesc Expression
derive instance genericOrdering :: Generic Ordering _
derive instance eqOrdering :: Eq Ordering
instance showOrdering :: Show Ordering where show x = genericShow x

-- TODO instance IsSql92OrderingSyntax Ordering where

newtype Grouping = Grouping (Array Expression)
derive instance genericGrouping :: Generic Grouping _
derive instance eqGrouping :: Eq Grouping
instance showGrouping :: Show Grouping where show x = genericShow x

-- TODO instance IsSql92GroupingSyntax Grouping where

data TableName = TableName (Maybe Text) Text
derive instance genericTableName :: Generic TableName _
derive instance eqTableName :: Eq TableName
derive instance ordTableName :: Ord TableName
instance showTableName :: Show TableName where show x = genericShow x

-- TODO instance IsSql92TableNameSyntax TableName where

data TableSource
  = TableNamed TableName
  | TableFromSubSelect Select
  | TableFromValues (Array (Array Expression))
derive instance genericTableSource :: Generic TableSource _
derive instance eqTableSource :: Eq TableSource
instance showTableSource :: Show TableSource where show x = genericShow x

-- TODO instance IsSql92TableSourceSyntax TableSource where

data From
  = FromTable TableSource (Maybe (Text /\ Maybe (Array Text)))
  | InnerJoin From From (Maybe Expression)
  | LeftJoin From From (Maybe Expression)
  | RightJoin From From (Maybe Expression)
  | OuterJoin From From (Maybe Expression)
derive instance genericFrom :: Generic From _
derive instance eqFrom :: Eq From
instance showFrom :: Show From where show x = genericShow x

-- TODO instance IsSql92FromSyntax From where

data Value
  = Value (Exists ValueF)
data ValueF a = ValueF a

-- TODO
-- #define VALUE_SYNTAX_INSTANCE(ty)
--   instance HasSqlValueSyntax Value ty where
--     sqlValueSyntax = Value
-- VALUE_SYNTAX_INSTANCE(Int)
-- VALUE_SYNTAX_INSTANCE(Int16)
-- VALUE_SYNTAX_INSTANCE(Int32)
-- VALUE_SYNTAX_INSTANCE(Int64)
-- VALUE_SYNTAX_INSTANCE(Word)
-- VALUE_SYNTAX_INSTANCE(Word16)
-- VALUE_SYNTAX_INSTANCE(Word32)
-- VALUE_SYNTAX_INSTANCE(Word64)
-- VALUE_SYNTAX_INSTANCE(Integer)
-- VALUE_SYNTAX_INSTANCE(String)
-- VALUE_SYNTAX_INSTANCE(Text)
-- VALUE_SYNTAX_INSTANCE(ByteString)
-- VALUE_SYNTAX_INSTANCE(LocalTime)
-- VALUE_SYNTAX_INSTANCE(UTCTime)
-- VALUE_SYNTAX_INSTANCE(Day)
-- VALUE_SYNTAX_INSTANCE(TimeOfDay)
-- VALUE_SYNTAX_INSTANCE(SqlNull)
-- VALUE_SYNTAX_INSTANCE(Double)
-- VALUE_SYNTAX_INSTANCE(Bool)

-- TODO instance HasSqlValueSyntax Value x => HasSqlValueSyntax Value (Maybe x) where
-- TODO
instance eqValue :: Eq Value where
  eq _ _ = true
-- TODO
instance showValue :: Show Value where
  show _ = ""

newtype WindowFrame = WindowFrame
  { partitions :: Maybe (Array Expression)
  , ordering :: Maybe (Array Ordering)
  , bounds :: Maybe WindowFrameBounds
  }
derive instance genericWindowFrame :: Generic WindowFrame _
derive instance eqWindowFrame :: Eq WindowFrame
instance showWindowFrame :: Show WindowFrame where show x = genericShow x

-- TODO instance IsSql2003WindowFrameSyntax WindowFrame where

newtype WindowFrameBounds = WindowFrameBounds
  { from :: WindowFrameBound
  , to :: Maybe WindowFrameBound
  }
derive instance genericWindowFrameBounds :: Generic WindowFrameBounds _
derive instance eqWindowFrameBounds :: Eq WindowFrameBounds
instance showWindowFrameBounds :: Show WindowFrameBounds where show x = genericShow x

-- TODO instance IsSql2003WindowFrameBoundsSyntax WindowFrameBounds where

data WindowFrameBound
  = WindowFrameUnbounded
  | WindowFrameBoundNRows Int
derive instance genericWindowFrameBound :: Generic WindowFrameBound _
derive instance eqWindowFrameBound :: Eq WindowFrameBound
instance showWindowFrameBound :: Show WindowFrameBound where show x = genericShow x

-- TODO instance IsSql2003WindowFrameBoundSyntax WindowFrameBound where

