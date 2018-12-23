module Format where

import Prelude
import Type.Data.Symbol (SProxy)
import Prim.Symbol as Symbol

-- | create a new Kind (type-level Union), Fmt, to represent Format Token
--   kind Fmt
--     = Var Symbol
--     | Lit Symbol
foreign import kind Fmt
foreign import data Var :: Symbol -> Fmt
foreign import data Lit :: Symbol -> Fmt


-- | use recursive Type constructor to carry a list of Fmt
--   kind FList
--     = FNil
--     | FCons Fmt FList
foreign import kind FList
foreign import data FNil :: FList
foreign import data FCons :: Fmt -> FList -> FList

-- | a value whose Type carries a FList
-- | different definition from purescript-variant but the same idea
data FProxy (fl :: FList) = FProxy

-- | A template literal has a canonical representation as a list of Format Tokens.
-- | e.g.
-- |   "/user/create/{name}/{age}" -> [Lit "/user/create/", Var "name", Var "age"]
-- | Therefore, use functional dependency to establish such a unique mapping:
class Parse (string :: Symbol) (fl :: FList) | string -> fl

class ParseLit (h :: Symbol) (t :: Symbol) (o :: FList) | h t -> o
class ParseVar (h :: Symbol) (t :: Symbol) (var :: Fmt) (rest :: Symbol) | h t -> var rest

-- | A list of Format Tokens can be uniquely mapped to a Row
-- | e.g.
-- | [Lit "/user/create/", Var "name", Var "age"]
-- |   -> forall t0 t1 r.
-- |      { name :: t0
-- |      , age :: t1
-- |      | r
-- |      }
-- | Only match the field names so Types are bounded by universal quantifiers.
class FormatParsed (fl :: FList) (row :: # Type) where
  formatParsed :: FProxy fl -> Record row -> String

  -- | Compose (string -> fl) and (fl -> row), we have (string -> row).
class Format (string :: Symbol) (row :: # Type) where
  format :: SProxy string -> Record row -> String

instance formatParsedFormat ::
  ( Parse string fl
  , FormatParsed fl row
  ) => Format string row where
  format _ = formatParsed (FProxy :: FProxy fl)
