module Database.Beam.Backend.SQL.AST where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

-- | Type Alias
type Bool = Boolean
type Integer = Int -- unbounded integer
type Word = Int -- unsigned bounded integer
type Text = String

-- | Ordering
data Ordering
  = OrderingAsc Expression
  | OrderingDesc Expression

-- | Expression

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

  | ExpressionCase (Array (Tuple Expression Expression)) Expression
  | ExpressionCoalesce (Array Expression)
  | ExpressionNullIf Expression Expression

  | ExpressionFieldName FieldName

  | ExpressionBetween Expression Expression Expression
  | ExpressionBinOp Text Expression Expression
  | ExpressionCompOp Text (Maybe ComparatorQuantifier) Expression Expression
  | ExpressionUnOp Text Expression

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

-- | Expression > Value
data Value

-- data Value where
--   Value :: (Show a, Eq a, Typeable a) => a -> Value

-- #define VALUE_SYNTAX_INSTANCE(ty) instance HasSqlValueSyntax Value ty where { sqlValueSyntax = Value }
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

-- instance HasSqlValueSyntax Value x => HasSqlValueSyntax Value (Maybe x) where
--   sqlValueSyntax (Just x) = sqlValueSyntax x
--   sqlValueSyntax Nothing = sqlValueSyntax SqlNull

-- instance Eq Value where
--   Value a == Value b =
--     case cast a of
--       Just a' -> a' == b
--       Nothing -> False
-- instance Show Value where
--   showsPrec prec (Value a) =
--     showParen (prec > app_prec) $
--     ("Value " ++ ).
--     showsPrec (app_prec + 1) a
--     where app_prec = 10


-- | Expression > FieldName
data FieldName
  = QualifiedField Text Text
  | UnqualifiedField Text

-- | Expression > ComparatorQuantifier
data ComparatorQuantifier
  = ComparatorQuantifierAny
  | ComparatorQuantifierAll

-- | Expression > CastTarget
data CastTarget
  = CastTargetDataType DataType
  | CastTargetDomainName Text

-- | Expression > CastTarget > DataType
data DataType
  = DataTypeChar Bool {- Varying -} (Maybe Word) (Maybe Text)
  | DataTypeNationalChar Bool (Maybe Word)
  | DataTypeBit Bool (Maybe Word)
  | DataTypeNumeric (Maybe (Tuple Word (Maybe Word)))
  | DataTypeDecimal (Maybe (Tuple Word (Maybe Word)))
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
  | DataTypeRow (Array (Tuple Text DataType))

  | DataTypeDomain Text

-- | Expression > CastTarget > DataType > ExtractField
data ExtractField
  = ExtractFieldTimeZoneHour
  | ExtractFieldTimeZoneMinute

  | ExtractFieldDateTimeYear
  | ExtractFieldDateTimeMonth
  | ExtractFieldDateTimeDay
  | ExtractFieldDateTimeHour
  | ExtractFieldDateTimeMinute
  | ExtractFieldDateTimeSecond

-- | Expression > WindowFrame
newtype WindowFrame = WindowFrame
  { windowFramePartitions :: Maybe (Array Expression)
  , windowFrameOrdering   ::  Maybe (Array Ordering)
  , windowFrameBounds     :: Maybe WindowFrameBounds
  }

-- | Expression > WindowFrame > WindowFrameBounds
newtype WindowFrameBounds = WindowFrameBounds
    { boundsFrom :: WindowFrameBound
    , boundsTo   :: Maybe WindowFrameBound
    }

-- | Expression > WindowFrame > WindowFrameBounds > WindowFrameBound
data WindowFrameBound
  = WindowFrameUnbounded
  | WindowFrameBoundNRows Int

-- | Command (Select, Insert, Update, Delete)

data Command
  = SelectCommand Select
  -- | InsertCommand Insert
  -- | UpdateCommand Update
  -- | DeleteCommand Delete

-- | Select
newtype Select = Select
  { selectTable :: SelectTable
  , selectOrdering   ::  Array Ordering
  , selectLimit :: Maybe Integer
  , selectOffset :: Maybe Integer
  }


-- | Select > SelectTable
data SelectTable
  = SelectTable
    { selectQuantifier :: Maybe SetQuantifier
    , selectProjection :: Projection
    , selectFrom       :: Maybe From
    , selectWhere      :: Maybe Expression
    , selectGrouping   :: Maybe Grouping
    , selectHaving     :: Maybe Expression
    }
  | UnionTables Bool SelectTable SelectTable
  | IntersectTables Bool SelectTable SelectTable
  | ExceptTable Bool SelectTable SelectTable

-- | Select > SelectTable > SetQuantifier
data SetQuantifier
  = SetQuantifierAll
  | SetQuantifierDistinct

-- | Select > SelectTable > Projection
newtype Projection
  = ProjExprs (Array (Tuple Expression (Maybe Text)))

-- | Select > SelectTable > From
data From
  = FromTable TableSource (Maybe Text)
  | InnerJoin From From (Maybe Expression)
  | LeftJoin From From (Maybe Expression)
  | RightJoin From From (Maybe Expression)
  | OuterJoin From From (Maybe Expression)

-- | Select > SelectTable > From > TableSource
data TableSource
  = TableNamed Text
  | TableFromSubSelect Select

-- | Select > SelectTable > Grouping
newtype Grouping = Grouping (Array Expression)

-- | Insert
newtype Insert = Insert
  { insertTable :: Text
  , insertFields :: Array Text
  , insertValues :: InsertValues
  }

-- | Insert > InsertValues
data InsertValues
  = InsertValues { insertValuesExpressions :: Array (Array Expression) }
  | InsertSelect { insertSelectStmt :: Select }

-- | Update
newtype Update = Update
  { updateTable :: Text
  , updateFields :: Array (Tuple FieldName Expression)
  , updateWhere :: Maybe Expression
  }

-- | Delete
newtype Delete = Delete
  { deleteTable :: Text
  , deleteAlias :: Maybe Text
  , deleteWhere :: Maybe Expression
  }
