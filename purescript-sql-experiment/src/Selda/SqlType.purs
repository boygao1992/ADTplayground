module Selda.SqlType where

import Prelude
import Type.Prelude (Proxy)

import Data.DateTime (DateTime, Time)
import Data.Enum (class BoundedEnum)
import Data.Exists (Exists)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe)
import Data.UUID (UUID)

-- | Format string used to represent date and time when
--   representing timestamps as text.
--   If at all possible, use 'SqlUTCTime' instead.
sqlDateTimeFormat :: String
sqlDateTimeFormat = "%F %H:%M:%S%Q%z"

-- | Format string used to represent date when
--   representing dates as text.
--   If at all possible, use 'SqlDate' instead.
sqlDateFormat :: String
sqlDateFormat = "%F"

-- | Format string used to represent time of day when
--   representing time as text.
--   If at all possible, use 'SqlTime' instead.
sqlTimeFormat :: String
sqlTimeFormat = "%H:%M:%S%Q%z"

data SqlTypeRep
  = TText
  | TRowID
  | TInt
  | TFloat
  | TBool
  | TDateTime
  | TDate
  | TTime
  | TBlob
  | TUUID
  | TJSON
derive instance genericSqlTypeRep :: Generic SqlTypeRep _
instance showSqlTypeRep :: Show SqlTypeRep where show x = genericShow x
instance eqSqlTypeRep :: Eq SqlTypeRep where eq x y = genericEq x y
instance ordSqlTypeRep :: Ord SqlTypeRep where compare x y = genericCompare x y

data Lit a
  -- LText     :: !Text       -> Lit Text
  = LText      String   (a ~ String)
  -- LInt      :: !Int        -> Lit Int
  | LInt       Int      (a ~ Int)
  -- LDouble   :: !Double     -> Lit Double
  | LDouble    Number   (a ~ Number)
  -- LBool     :: !Bool       -> Lit Bool
  | LBool      Boolean  (a ~ Boolean)
  -- LDateTime :: !UTCTime    -> Lit UTCTime
  | LDateTime  DateTime (a ~ DateTime)
  -- TODO LDate     :: !Day        -> Lit Day
  -- | LDate      Day      (a ~ Day)
  -- LTime     :: !TimeOfDay  -> Lit TimeOfDay
  | LTime      Time     (a ~ Time)
  -- LJust     :: SqlType a => !(Lit a) -> Lit (Maybe a)
  | LJust      (Lit a)  (SqlType a => a ~ (Maybe a))
  -- TODO LBlob     :: !ByteString -> Lit ByteString
  -- LNull     :: SqlType a => Lit (Maybe a)
  | LNull               (SqlType a => a ~ (Maybe a))
  -- LCustom   :: SqlTypeRep  -> Lit a -> Lit b
  | LCustom    SqlTypeRep (Exists Lit) a
  -- LUUID     :: !UUID       -> Lit UUID
  | LUUID      UUID     (a ~ UUID)

data SqlValue
  = SqlInt     Int
  | SqlFloat   Number -- Double
  | SqlString  String -- Text
  | SqlBool    Boolean
  -- TODO | SqlBlob    ByteString
  | SqlUTCTime DateTime
  | SqlTime    Time
  -- TODO | SqlDate    Day
  | SqlNull

class SqlType a where
  -- | Create a literal of this type.
  mkLit :: a -> Lit a
  -- default mkLit :: (Typeable a, SqlEnum a) => a -> Lit a
  -- mkLit = LCustom TText . LText . toText

  -- | The SQL representation for this type.
  sqlType :: Proxy a -> SqlTypeRep
  -- sqlType _ = litType (defaultValue :: Lit a)

  -- | Convert an SqlValue into this type.
  fromSql :: SqlValue -> a
  -- default fromSql :: (Typeable a, SqlEnum a) => SqlValue -> a
  -- fromSql = fromText . fromSql

  -- | Default value when using 'def' at this type.
  defaultValue :: Lit a
  -- default defaultValue :: (Typeable a, SqlEnum a) => Lit a
  -- defaultValue = mkLit (minBound :: a)

class BoundedEnum a <= SqlEnum a where
  toText :: a -> String
  fromText :: String -> a

-- | A row identifier for some table.
--   This is the type of auto-incrementing primary keys.
newtype RowID = RowID Int
derive instance genericRowID :: Generic RowID _
derive instance eqRowID :: Eq RowID
derive instance ordRowID :: Ord RowID
derive newtype instance showRowID :: Show RowID

newtype ID a = ID RowID
derive instance eqID :: Eq (ID a)
derive instance ordID :: Ord (ID a)
derive newtype instance showID :: Show (ID a)
