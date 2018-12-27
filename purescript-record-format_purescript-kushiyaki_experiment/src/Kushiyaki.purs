module Kushiyaki.Annotated where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, splitAt, stripPrefix)
import Global (readInt)
import Format (class Parse, FCons, FLProxy(..), FNil, Lit, Var, kind FList)
import Prim.Row as Row
import Prim.Symbol (class Cons, class Append) as Symbol
import Prim.TypeError as TE
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Utils (class RemoveSpace)

type Url = String
type Error = String

class ParseUrl (template :: Symbol) (row :: # Type) | template -> row where
  parseUrl :: SProxy template -> Url -> Either Error (Record row)

instance parseUrlAll ::
  ( Parse template fl -- (url :: Symbol) -> (fl :: FList)
  , ParseUrlImpl fl () row -- (fl :: FList) -> ( () :: # Type) (row :: # Type)
  ) => ParseUrl template row
  where
    parseUrl _ = map (\record -> Builder.build record {})
                 <<< parseUrlImpl (FLProxy :: FLProxy fl)

class ParseUrlImpl (xs :: FList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  parseUrlImpl
    :: FLProxy xs
    -> Url
    -> Either Error (Builder (Record from) (Record to))

instance parseUrlImplNil :: ParseUrlImpl FNil () () where
  parseUrlImpl _ _ = pure identity

instance parseUrlImplFConsVar ::
  ( RemoveSpace var varNoSpace
  , ParseTypedParam varNoSpace name ty -- (var :: Symbol) -> (name :: Symbol) (ty :: Type)
  , ParseUrlImpl restFl from restTo
  , Row.Cons name ty restTo to
  , Row.Lacks name restTo -- no replicate field names
  , IsSymbol name -- for Builder.insert
  , ReadParam ty -- infer a value-level function, readParam
  ) => ParseUrlImpl (FCons (Var var) restFl) from to
  where
    parseUrlImpl _ s =
      let
        label = SProxy :: SProxy name
        split = case indexOf (Pattern "/") s of
          Nothing -> { before : s, after : "" } -- EOL
          -- "wenbo/44" -> {before: "wenbo", after: "/44"}
          Just idx -> splitAt idx s
      in
        do
          value <- readParam split.before
          restBuilder <- parseUrlImpl (FLProxy :: FLProxy restFl) split.after
          pure $ (Builder.insert label value) <<< restBuilder

instance parseUrlImplFConsLit ::
  ( ParseUrlImpl restFl from to
  , IsSymbol lit
  ) => ParseUrlImpl (FCons (Lit lit) restFl) from to
  where
    parseUrlImpl _ s =
      let
        lit = reflectSymbol (SProxy :: SProxy lit)
      in
        case stripPrefix (Pattern lit) s of
          Nothing ->
            Left $ "could not strip segment" <> lit <> " from path " <> s
          Just rest ->
            parseUrlImpl (FLProxy :: FLProxy restFl) rest


class ParseTypedParam (s :: Symbol) (name :: Symbol) (ty :: Type) | s -> name ty
instance parseTypedParamAll ::
  ( Symbol.Cons x xs s -- (x :: Symbol) (xs :: Symbol) <- s
  , ParseTypedParamImpl x xs "" name ty -- x xs "" -> name ty
  ) => ParseTypedParam s name ty

class ParseTypedParamImpl
      (x :: Symbol) (xs :: Symbol) (acc :: Symbol)
      (name :: Symbol) (ty :: Type)
  | x xs acc -> name ty

instance parseTypedParamImplColon ::
  ( MatchTypeName tyName ty
  ) => ParseTypedParamImpl ":" tyName name name ty
else instance parseTypedParamImplNoTypeAnnotation ::
  ( Symbol.Append acc x name -- acc x -> name
  ) => ParseTypedParamImpl x "" acc name String -- default type, String
else instance parseTypedParamImplRecursion ::
  ( Symbol.Cons xs_h xs_t xs -- xs_h xs_t <- xs
  , Symbol.Append acc x acc' -- acc x -> acc'
  , ParseTypedParamImpl xs_h xs_t acc' name ty
  ) => ParseTypedParamImpl x xs acc name ty

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

class ReadParam a where
  readParam :: String -> Either String a

instance readParamString :: ReadParam String where
  readParam = pure

instance readParamInt :: ReadParam Int where
  readParam s = case fromNumber (readInt 10 s) of
    Nothing -> Left $ "could not parse " <> s <> " into Int"
    Just x -> Right x
