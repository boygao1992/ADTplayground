module Selda.Generic where

import Prelude
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), RProxy(..), SProxy(..), reflectSymbol)

import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList

import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Read.Enum (genericReadEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String.Read (class Read, read)
import Effect (Effect)
import Effect.Exception (message, try)
import Effect.Exception.Unsafe (unsafeThrow, unsafeThrowException)
import Effect.Unsafe (unsafePerformEffect)
import Record as Record

import Selda.Exp (UntypedCol, col, lit, untyped)
import Selda.SQL (Param, mkParam)
import Selda.SqlType (class SqlType, Lit, defaultValue, mkLit, sqlType)
import Selda.Table.Type (ColAttr(..), ColInfo)
import Selda.Types (ColName, fromColName, mkColName)

-- | Any type which has a corresponding relation.
--   To make a @Relational@ instance for some type, simply derive 'Generic'.
--
--   Note that only types which have a single data constructor, and where all
--   fields are instances of 'SqlValue' can be used with this module.
--   Attempting to use functions in this module with any type which doesn't
--   obey those constraints will result in a very confusing type error.
-- type Relational a =
--   ( Generic a
--   , SqlRow a
--   , GRelation (Rep a)
--   )
-- NOTE missing constraint kind

-- | Extract all insert parameters from a generic value.
params :: forall a. GRelation a => a -> Array (Either Param Param)
params = unsafePerformEffect <<< gParams

-- | Extract all column names from the given type.
--   If the type is not a record, the columns will be named @col_1@,
--   @col_2@, etc.
tblCols :: forall a. GRelation a => Proxy a -> (String -> String) -> Array ColInfo
tblCols a fieldMod = gTblCols a (mkColName <<< fieldMod <<< fromColName)

-- | Exception indicating the use of a default value.
--   If any values throwing this during evaluation of @param xs@ will be
--   replaced by their default value.
data DefaultValueException = DefaultValueException
derive instance genericDefaultValueException :: Generic DefaultValueException _
instance showDefaultValueException :: Show DefaultValueException where
  show = genericShow
instance readDefaultValueException :: Read DefaultValueException where
  read = genericReadEnum
-- instance Exception DefaultValueException
-- NOTE missing Exception hierarchy

def :: forall a. SqlType a => a
def = unsafeThrow $ show $ DefaultValueException

-- NOTE mod: Generic a rep => Newtype a (Record row)
class GRelation a where
  -- | Generic worker for 'params'.
  gParams :: a -> Effect (Array (Either Param Param))
  -- NOTE Left DefaultValueException -> Left $ Param (defaultValue :: Lit a)
  -- def :: SqlType a => a
  -- def = throw DefaultValueException

  -- | Compute all columns needed to represent the given type.
  gTblCols
    :: Proxy a
    -> (ColName -> ColName)
    -> Array ColInfo

  -- | Create a new value with all default fields.
  gNew :: forall sql. Proxy a -> Array (UntypedCol sql)

instance gRelationInit ::
  ( Newtype a (Record row)
  , RowList.RowToList row rl
  , GRelationRL rl row
  )
  => GRelation a where
  gParams a = gParamsRL (RLProxy :: RLProxy rl) (unwrap a)
  gTblCols _ rename = gTblColsRL (RLProxy :: RLProxy rl) (RProxy :: RProxy row) rename
  gNew _ = gNewRL (RLProxy :: RLProxy rl) (RProxy :: RProxy row)

class GRelationRL (rl :: RowList) (row :: # Type) where
  gParamsRL :: RLProxy rl -> Record row -> Effect (Array (Either Param Param))
  gTblColsRL
    :: RLProxy rl
    -> RProxy row
    -> (ColName -> ColName)
    -> Array ColInfo
  gNewRL :: forall sql. RLProxy rl -> RProxy row -> Array (UntypedCol sql)

instance gRelationRLNil ::
  GRelationRL RowList.Nil row where
  gParamsRL _ _ = pure []
  gTblColsRL _ _ _ = []
  gNewRL _ _ = []

instance gRelationRLCons ::
  ( GRelationRL restRL row
  , IsSymbol label
  , Row.Cons label a restRow row
  , SqlType a
  )
  => GRelationRL (RowList.Cons label a restRL) row where
  gParamsRL _ r =
    let
      x = Record.get (SProxy :: SProxy label) r
    in
      lift2 (<>) (gParamsRL (RLProxy :: RLProxy restRL) r) do
        res <- try $ pure x
        pure case res of
          Right x' -> [Right $ mkParam $ mkLit x']
          Left err -> case read $ message err of
            Nothing -> unsafeThrowException err
            Just (_ :: DefaultValueException) ->
              [Left $ mkParam (defaultValue :: Lit a)]

  gTblColsRL _ row rename =
    let name = rename $ mkColName $ reflectSymbol (SProxy :: SProxy label)
    in gTblColsRL (RLProxy :: RLProxy restRL) row rename
        <> [{ colName: name
            , colType: sqlType (Proxy :: Proxy a)
            , colAttrs: gColAttrs (Proxy :: Proxy a)
            , colFKs: []
            , colExpr: untyped $ col $ name
            }]

  gNewRL _ row
    = [untyped $ lit (defaultValue :: Lit a)]
    <> gNewRL (RLProxy :: RLProxy restRL) row

class GColAttrs a where
  gColAttrs :: Proxy a -> Array ColAttr

instance gColAttrsMaybe :: GColAttrs (Maybe a) where
  gColAttrs _ = [Optional]
else
instance gColAttrsNonMaybe :: GColAttrs a where
  gColAttrs _ = [Required]


{- GHC.Generic
-- NOTE Constructor name NoArguments
-- | Unit: used for constructors without arguments
data U1 p = U1

-- NOTE K1 P c p ~ Argument (non-recursive)
-- NOTE K1 R c p ~ Argument (recursive)
-- | Constants, additional parameters and recursion of kind *
newtype K1 i c p = K1 { unK1 :: c }

-- NOTE M1 D c f p (datatype) ~ no correspondence
-- NOTE M1 C c f p ~ Constructor
-- NOTE M1 S NoSelector f p ~ an extra construct between Constructor and Argument
-- NOTE M1 S selector f p ~ (record fields)
-- | Meta-information (constructor names, etc.)
newtype M1 i c f p = M1 { unM1 :: f p }

-- NOTE Sum
-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

-- NOTE Product
-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g p = f p :*: g p

-- NOTE example
data UserTree a = Leaf | Node a (UserTree a) (UserTree a)

type RealRepUserTree a =
  -- Information about the datatype
  M1 D Data_UserTree
  (
  -- Leaf, with information about the constructor
      M1 C Con_Leaf U1
  -- Node, with information about the constructor
  :+: M1 C Con_Node
      (
        -- Constructor argument, which could have information
        -- about a record selector label
            M1 S NoSelector
            (
              -- Argument, tagged with P because it is a parameter
              K1 P a
            )
        -- Another argument, tagged with R because it is
        -- a recursive occurrence of a type
        :*: M1 S NoSelector (K1 R (UserTree a))
        -- Idem
        :*: M1 S NoSelector (K1 R (UserTree a))
      )
  )
-}

{- Spec
data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Pet
  }
  deriving Generic -- NOTE Constructor "Person" (Argument (Record row))
instance SqlRow Person

people :: Table Person
people = table "people" [#name :- primary]
-}
