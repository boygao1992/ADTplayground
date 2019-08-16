module Selda.SqlType where

import Prelude
import Type.Prelude

import Effect.Exception.Unsafe
import Data.DateTime
import Data.Enum (class BoundedEnum)
import Data.Exists
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Leibniz
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Type.Prelude (Proxy)

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
derive instance eqSqlTypeRep :: Eq SqlTypeRep
derive instance ordSqlTypeRep :: Ord SqlTypeRep
instance showSqlTypeRep :: Show SqlTypeRep where show = genericShow

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

-- | Any type that's bounded, enumerable and has a text representation, and
--   thus representable as a Selda enumerable.
--
--   While it would be more efficient to store enumerables as integers, this
--   makes hand-rolled SQL touching the values inscrutable, and will break if
--   the user a) derives Enum and b) changes the order of their constructors.
--   Long-term, this should be implemented in PostgreSQL as a proper enum
--   anyway, which mostly renders the performance argument moot.
class BoundedEnum a <= SqlEnum a where
  toText :: a -> String
  fromText :: String -> a

-- TODO instance (BoundedEnum a, Show a, Read a) => SqlEnum a

-- | An SQL literal.
data Lit a
  = LText      (a ~ String)   String
  | LInt       (a ~ Int)      Int
  | LDouble    (a ~ Number)   Number
  | LBool      (a ~ Boolean)  Boolean
  | LDateTime  (a ~ DateTime) DateTime
  | LDate      (a ~ Day)      Day -- TODO different semantics
  | LTime      (a ~ Time)     Time
  -- TODO LBlob     :: !ByteString -> Lit ByteString
  | LJust      (Exists (LJustF a))
  | LNull      (Exists (LNullF a))
  | LCustom    SqlTypeRep (Exists Lit)
  | LUUID      (a ~ UUID) UUID
data LJustF a b = LJustF (a ~ (Maybe b)) (Lit b)
data LNullF a b = LNullF (a ~ (Maybe b)) (Proxy b -> SqlTypeRep) -- NOTE `SqlType b` constraint cannot survive `mkExists` (which uses unsafeCoerce under the hood), so we box the `sqlType` function during construction
-- Proxy a -> SqlTypeRep
-- LText     :: !Text       -> Lit Text
lText = LText identity :: String -> Lit String
-- LInt      :: !Int        -> Lit Int
lInt = LInt identity :: Int -> Lit Int
-- LDouble   :: !Double     -> Lit Double
lDouble = LDouble identity :: Number -> Lit Number
-- LBool     :: !Bool       -> Lit Bool
lBool = LBool identity :: Boolean -> Lit Boolean
-- LDateTime :: !UTCTime    -> Lit UTCTime
lDateTime = LDateTime identity :: DateTime -> Lit DateTime
-- LDate     :: !Day        -> Lit Day
lDate = LDate identity :: Day -> Lit Day
-- LTime     :: !TimeOfDay  -> Lit TimeOfDay
lTime = LTime identity :: Time -> Lit Time
-- LJust     :: SqlType a => !(Lit a) -> Lit (Maybe a)
lJust :: forall a. SqlType a => Lit a -> Lit (Maybe a)
lJust = LJust <<< mkExists <<< LJustF identity
-- LNull     :: SqlType a => Lit (Maybe a)
lNull :: forall a. SqlType a => Lit (Maybe a)
lNull = LNull $ mkExists $ LNullF identity sqlType
-- LCustom   :: SqlTypeRep  -> Lit a -> Lit b
lCustom :: forall a b. SqlTypeRep -> Lit a -> Lit b
lCustom rep = LCustom rep <<< mkExists
-- LUUID     :: !UUID       -> Lit UUID
lUUID = LUUID identity :: UUID -> Lit UUID

-- TODO | The SQL type representation for the given literal.
litType :: forall a. Lit a -> SqlTypeRep
litType (LText _ _)     = TText
litType (LInt _ _)      = TInt
litType (LDouble _ _)   = TFloat
litType (LBool _ _)     = TBool
litType (LDateTime _ _) = TDateTime
litType (LDate _ _)     = TDate
litType (LTime _ _)     = TTime
-- litType (LBlob _)     = TBlob
litType (LJust lJustF) = flippedRunExists lJustF \(LJustF proof x) -> litType x -- NOTE proof ignored
litType (LNull lNullF) = flippedRunExists lNullF \(LNullF proof f) ->
  f (proxyFor proof)
  where
    -- NOTE mod
    proxyFor :: forall b. a ~ (Maybe b) -> Proxy b
    proxyFor (Leibniz _) = Proxy :: Proxy b
litType (LCustom t _) = t
litType (LUUID _ _)     = TUUID

-- TODO instance Eq (Lit a) where
-- TODO instance Ord (Lit a) where
-- TODO instance Show (Lit a) where

-- | Constructor tag for all literals. Used for Ord instance.
-- TODO litConTag :: Lit a -> Int
-- | Compare two literals of different type for equality.
-- TODO compLit :: Lit a -> Lit b -> Ordering

data SqlValue
  = SqlInt     Int
  | SqlFloat   Number -- Double
  | SqlString  String -- Text
  | SqlBool    Boolean
  -- TODO | SqlBlob    ByteString
  | SqlUTCTime DateTime
  | SqlTime    Time
  | SqlDate    Day
  | SqlNull
derive instance genericSqlValue :: Generic SqlValue _
instance showSqlValue :: Show SqlValue where
  show = genericShow

-- | A row identifier for some table.
--   This is the type of auto-incrementing primary keys.
newtype RowID = RowID Int
derive instance eqRowID :: Eq RowID
derive instance ordRowID :: Ord RowID
derive newtype instance showRowID :: Show RowID

-- | A row identifier which is guaranteed to not match any row in any table.
invalidRowId :: RowID
invalidRowId = RowID (-1)

-- | Is the given row identifier invalid? I.e. is it guaranteed to not match any
--   row in any table?
isInvalidRowId :: RowID -> Boolean
isInvalidRowId (RowID n) = n < 0

-- | Create a row identifier from an integer.
--   Use with caution, preferably only when reading user input.
toRowId :: Int -> RowID
toRowId = RowID

-- | Inspect a row identifier.
fromRowId :: RowID -> Int
fromRowId (RowID n) = n

-- | A typed row identifier.
--   Generic tables should use this instead of 'RowID'.
--   Use 'untyped' to erase the type of a row identifier, and @cast@ from the
--   "Database.Selda.Unsafe" module if you for some reason need to add a type
--   to a row identifier.
newtype ID a = ID RowID
derive instance eqID :: Eq (ID a)
derive instance ordID :: Ord (ID a)
derive newtype instance showID :: Show (ID a)
-- | Create a typed row identifier from an integer.
--   Use with caution, preferably only when reading user input.
toId :: forall a. Int -> ID a
toId = ID <<< toRowId

-- | Create a typed row identifier from an integer.
--   Use with caution, preferably only when reading user input.
fromId :: forall a. ID a -> Int
fromId (ID i) = fromRowId i

-- | A typed row identifier which is guaranteed to not match any row in any
--   table.
invalidId :: forall a. ID a
invalidId = ID invalidRowId

-- | Is the given typed row identifier invalid? I.e. is it guaranteed to not
--   match any row in any table?
isInvalidId :: forall a. ID a -> Boolean
isInvalidId (ID rowID)= isInvalidRowId rowID

-- TODO SqlType instances

instance sqlTypeRowID :: SqlType RowID where
  mkLit (RowID n) = lCustom TRowID (lInt n)
  sqlType _ = TRowID
  fromSql (SqlInt x) = RowID x
  fromSql v = unsafeThrow $ "fromSql: RowID column with non-int value: " <> show v
  defaultValue =
    let (RowID n) = invalidRowId
    in lCustom TRowID (lInt n)

derive newtype instance sqlTypeId :: SqlType (ID a)

instance sqlTypeInt :: SqlType Int where
  mkLit x = lInt x
  sqlType _ = TInt
  fromSql (SqlInt x) = x
  fromSql v = unsafeThrow $ "fromSql: int column with non-int value: " <> show v
  defaultValue = lInt 0

instance sqlTypeNumber :: SqlType Number where
  mkLit = lDouble
  sqlType _ = TFloat
  fromSql (SqlFloat x) = x
  fromSql v = unsafeThrow $ "fromSql: float column with non-float value: " <> show v
  defaultValue = lDouble 0.0

instance sqlTypeString :: SqlType String where
  mkLit = lText
  sqlType _ = TText
  fromSql (SqlString x) = x
  fromSql v = unsafeThrow $ "fromSql: text column with non-text value: " <> show v
  defaultValue = lText ""

instance sqlTypeBoolean :: SqlType Boolean where
  mkLit x = lBool x
  sqlType _ = TBool
  fromSql (SqlBool x) = x
  fromSql (SqlInt 0)  = false -- NOTE implicit type coercion
  fromSql (SqlInt _)  = true  -- NOTE implicit type coercion
  fromSql v = unsafeThrow $ "fromSql: bool column with non-bool value: " <> show v
  defaultValue = lBool false

-- TODO instance SqlType DateTime
-- TODO instance SqlType Day
-- TODO instance SqlType Time

-- | Both PostgreSQL and SQLite to weird things with time zones.
--   Long term solution is to use proper binary types internally for
--   time values, so this is really just an interim solution.
-- TODO withWeirdTimeZone :: ParseTime t => String -> String -> Maybe t

-- TODO instance SqlType ByteString
-- TODO instance SqlType UUID

instance sqlTypeMaybe :: SqlType a => SqlType (Maybe a) where
  mkLit (Just x) = lJust (mkLit x)
  mkLit Nothing  = lNull
  sqlType _ = sqlType (Proxy :: Proxy a)
  fromSql SqlNull = Nothing
  fromSql x       = Just $ fromSql x
  defaultValue = lNull

-- instance sqlTypeOrdering :: SqlType Ordering where
--   mkLit = lCustom TText <<< lText <<< toText
--   sqlType _ = litType
