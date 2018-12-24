module Kushiyaki.Annotated where

import Prelude

import Data.Either (Either)
import Prim.Symbol (class Append) as Symbol
import Type.Data.Symbol (SProxy)
import Prim.TypeError as TE
import Format (kind FList, FLProxy)
import Record.Builder (Builder)

type Url = String
class ParseUrl (template :: Symbol) (row :: # Type) | template -> row where
  parseUrl :: SProxy template -> Url -> Either String { | row }

class ParseURLImpl (xs :: FList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  parseURLImpl
    :: FLProxy xs
    -> String
    -> Either String (Builder { | from } { | to })

class ParseTypedParam (s :: Symbol) (name :: Symbol) (ty :: Type) | s -> name ty

-- | MatchTypeName :: Symbol -> Type
class MatchTypeName (s :: Symbol) (ty :: Type) | s -> ty

-- | Build a dictionary of all possible mappings from an element in the input set to an element in the output set.
-- | This function is partial, whose domain doesn't cover all possible strings (but of course).
-- | If hit an instance outside the domain, raise a TypeError ("no type class instance" by default).
instance aMatchTypeName :: MatchTypeName "String" String
else instance bMatchTypeName :: MatchTypeName "Int" Int
-- | replace "no type class instance" to a user-defined error message
else instance errMatchTypeName ::
  (Symbol.Append "Can't match type annotation to type: " s msg
  , TE.Fail (TE.Text msg) -- type-level log effect to display errors on screen
  ) => MatchTypeName s ty

