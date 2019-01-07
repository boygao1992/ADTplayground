module B2CExample.Model where

-- PLAN
-- distributed interface through type classes
-- 1. derived GraphQLType Definition
--   - default to NotNullAble unless Maybe
--   - use Generic instance for Sum type
--     - DONE class IsEnumPred, to check whether a data-type is Enum
--       - if so, map it to GraphQLEnumType
-- 2. Description
--   - default = Nothing
--   - class GraphQLDescription to provide description for each field (Just)
--   - class EnforceDescription to enforce all fields have description (Just)
-- 3. Resolver
--   - Record as input so we can decide its dependencies (type class constraints) based on the shape of the Record
--     - { source, args, context, info }
--     - if context is missing, then no need to enforce `class OutputType` constraint
--       - `OutputType (ObjectType ctx a) ctx`


-- | GraphQLType Definition
class GraphQLType a

-- | Description
class GraphQLDescription a
-- DONE class EnumToDescriptionRow
-- DONE class RecordToDescriptionRow
-- e.g. Cardholder
-- i :: ( id :: String , cards :: Array Card, banks :: Array Bank )
-- o :: ( id :: Maybe String, cards :: Maybe String, banks :: Maybe String )

-- TODO purescript-typelevel-eval
-- type MapToMaybeString =  ToRow <<< May (Const (Maybe String)) <<< FromRow
-- Eval (MayToMaybeString (RProxy i)) (RProxy o) =>

-- | Resolver
class GraphQLResolver a

newtype Cardholder = Cardholder
  { id :: String
  , cards :: Array Card -- one-many edge, from Cardholder to Card
  , banks :: Array Bank -- one-many edge
  }

newtype Card = Card
  { id :: String
  , cardholder :: Cardholder -- one-one edge
  }

newtype Bank = Bank
  { id :: String
  , cardholders :: Array Cardholder -- one-many edge
  , merchants :: Array Merchant -- one-many edge
  }

newtype Merchant = Merchant
  { id :: String
  , banks :: Array Bank -- one-many edge
  , transactions :: Array Transaction -- one-many edge
  }

newtype Transaction = Transaction
  { id :: String
  , card :: Card -- one-one edge
  , merchant :: Merchant -- one-one edge
  , currencyCode :: CurrencyCode -- one-one edge
  }

data CardType
  = AmericanExpress
  | Discover
  | MasterCard
  | Visa

data CurrencyCode
  = USD
  | CNY
  | GBP
