module Format where

import Prelude
import Prim.Row as Row
import Prim.Symbol as Symbol
import Record as Record
import Type.Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

-- TODO add TypeError (class Fail) to refine those partial functions to total functions
-- import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
-- an example from purescript-graphql
-- | else instance objectTypeOutputTypeFail
-- |   :: Fail (Above
-- |     (Text "Cannot use object type with different context as field result.")
-- |     (Above
-- |       (Beside (Text "Expected context type: ") (Quote expected))
-- |       (Beside (Text "But received constext type: ") (Quote actual))))
-- |   => OutputType (ObjectType actual a) expected


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
data FLProxy (fl :: FList) = FLProxy

-- | A template literal has a canonical representation as a list of Format Tokens.
-- | e.g.
-- |   "/user/create/{name}/{age}" -> [Lit "/user/create/", Var "name", Var "age"]
-- | Therefore, use functional dependency to establish such a unique mapping:
class Parse (string :: Symbol) (fl :: FList) | string -> fl

class ParseLit (h :: Symbol) (t :: Symbol) (o :: FList) | h t -> o
class ParseVar (h :: Symbol) (t :: Symbol) (var :: Fmt) (rest :: Symbol) | h t -> var rest

instance aParse :: Parse "" FNil
else instance bParse ::
  ( Symbol.Cons h t i
  , ParseLit h t o
  ) => Parse i o

instance aParseLit :: ParseLit h "" (FCons (Lit o) FNil)
else instance bParseLit ::
  ( Symbol.Cons t_h t_t t
  , ParseVar t_h t_t (Var match) rest
  , Parse rest restFl
  ) => ParseLit "{" t (FCons (Lit "") (FCons (Var match) restFl)) -- extra (Lit "") as base case for cParseLit
else instance cParseLit ::
  ( Parse t (FCons (Lit restLit) restFl)
  , Symbol.Cons h restLit lit
  ) => ParseLit h t (FCons (Lit lit) restFl)

instance aParseVar :: ParseVar "" t (Var "") ""
else instance bParseVar :: ParseVar "}" t (Var "") t
else instance cParseVar :: ParseVar h "" (Var h) ""
else instance dParseVar ::
  ( Symbol.Cons t_h t_t t
  , ParseVar t_h t_t (Var restVar) rest
  , Symbol.Cons h restVar var
  ) => ParseVar h t (Var var) rest

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
  formatParsed :: FLProxy fl -> Record row -> String

-- base case
                                    -- if set row to be () here, then the input Row has to be closed
                                    --   which means the input Record has to exactly match elements in the parsed Format Token list
                                    -- currently, it's open, forall r. { | r}, which means we can throw in more fields than required
instance formatParsedNil :: FormatParsed FNil row where
  formatParsed _ _ = ""

instance formatParsedFConsVar ::
  ( IsSymbol key
  , Row.Cons key typ restRow row -- key type restRow <- row, the compiler (Prim) will prepare all possible destructuring for this type class so order of keys doesn't matter
  , FormatParsed restFl row
  , FormatVar typ
  ) => FormatParsed (FCons (Var key) restFl) row
  where
    formatParsed _ row =
      let
        var :: String
        var = formatVar $ Record.get (SProxy :: SProxy key) row

        rest :: String
        rest = formatParsed (FLProxy :: FLProxy restFl) row
      in
        var <> rest

instance formatParsedFConsLit ::
  ( IsSymbol lit
  , FormatParsed restFl row
  ) => FormatParsed (FCons (Lit lit) restFl) row
  where
    formatParsed _ row =
      let
        literal :: String
        literal = reflectSymbol (SProxy :: SProxy lit)

        rest :: String
        rest = formatParsed (FLProxy :: FLProxy restFl) row
      in
        literal <> rest

class FormatVar a where
  formatVar :: a -> String

instance aFormatVar :: FormatVar String where
  formatVar = identity
else instance bFormatVar :: Show a => FormatVar a where
  formatVar = show

  -- | Compose Parse(string -> fl) and FormatParsed(fl -> row)
  -- | we have Format(string -> row).
  -- | The value level is a little bit richer with String as the output.
class Format (string :: Symbol) (row :: # Type) | string -> row where
  format :: SProxy string -> Record row -> String

instance formatParsedFormat ::
  ( Parse string fl
  , FormatParsed fl row
  ) => Format string row where
  format _ = formatParsed (FLProxy :: FLProxy fl)
