module Selda.Generic where

import Prelude
import Type.Prelude

import Effect.Unsafe
import Data.Generic.Rep (class Generic, from)
import Control.Monad.State
import Effect
import Data.Maybe
import Data.Either
import Data.Variant.Internal

import Selda.Types
import Selda.SqlType
import Selda.SqlRow (class SqlRow)
import Selda.Table.Type
import Selda.SQL (Param (..))
import Selda.Exp (Exp (Col, Lit), UntypedCol (..))

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
-- TODO params :: Relational a => a -> [Either Param Param]
params :: forall a rep. Generic a rep => GRelation rep => a -> Array (Either Param Param)
params = unsafePerformEffect <<< gParams <<< from

-- | Extract all column names from the given type.
--   If the type is not a record, the columns will be named @col_1@,
--   @col_2@, etc.
-- TODO tblCols :: forall a. Relational a => Proxy a -> (Text -> Text) -> [ColInfo]

-- | Exception indicating the use of a default value.
--   If any values throwing this during evaluation of @param xs@ will be
--   replaced by their default value.
-- data DefaultValueException = DefaultValueException
-- instance Exception DefaultValueException
-- NOTE missing Exception hierarchy

class GRelation rep where
  -- | Generic worker for 'params'.
  gParams :: rep -> Effect (Array (Either Param Param))
  -- NOTE Left DefaultValueException -> Left $ Param (defaultValue :: Lit a)
  -- def :: SqlType a => a
  -- def = throw DefaultValueException

  -- | Compute all columns needed to represent the given type.
  gTblCols
    :: Proxy rep
    -> Maybe ColName
    -> (Int -> Maybe ColName -> ColName)
    -> State Int (Array ColInfo)

  -- | Create a new value with all default fields.
  gNew :: forall sql. Proxy rep -> Array (UntypedCol sql)

-- TODO GRelation instances

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
