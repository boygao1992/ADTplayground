module Kushiyaki.RuntimeParser where

import Prelude

import Data.Either (Either(..), either)
import Data.Map (Map, insert, empty, fromFoldable) as Row
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (uncons, singleton) as Symbol
import Data.Tuple (Tuple(..))
import Record.Format.RuntimeParser (FList(..), Fmt(..), parse)

type User
  = { name :: String
    , number :: Int
    }

sampleTemplate :: String
sampleTemplate = "/user/create/{name:String}/{number:Int}"

sampleUrl :: String
sampleUrl = "/user/create/Wenbo/44"

type Error = String
type Symbol = String
data Type_
  = String_
  | Int_

instance showType_ :: Show Type_ where
  show String_ = "String"
  show Int_ = "Int"

matchTypedName :: Symbol -> Either Error Type_
matchTypedName "String" = Right String_
matchTypedName "Int" = Right Int_
matchTypedName ty = Left $ "Can't match type annotation to type: " <> ty

parseTypedParam :: Symbol -> Either Error (Tuple Symbol Type_)
parseTypedParam s = case Symbol.uncons s of
  -- in the original implementation, this is not handled explicitly so it falls back to the default "no type class instance" TypeError
  Nothing ->
    Left "var cannot be empty."
  Just { head : h, tail : t } ->
    parseTypedParamImpl (Symbol.singleton h) t ""

    where
      parseTypedParamImpl :: Symbol -> Symbol -> Symbol -> Either Error (Tuple Symbol Type_)
      parseTypedParamImpl ":" tyName acc = case (matchTypedName tyName) of
        Left error -> Left error
        Right ty -> Right $ Tuple acc ty
      parseTypedParamImpl x xs acc = case Symbol.uncons xs of
        Nothing -> -- no type annotation
          Right $ Tuple (acc <> x) String_
        Just { head : y, tail : ys } ->
          parseTypedParamImpl (Symbol.singleton y) ys (acc <> x)

type Row_ = Row.Map Symbol Type_

type Template = String
type Url = String

parseUrl :: Template -> Either Error Row_
parseUrl template = parseUrlImpl xs Row.empty
  where
    xs :: FList
    xs = parse template

    parseUrlImpl :: FList -> Row_ -> Either Error Row_
    parseUrlImpl FNil row = Right row
    parseUrlImpl (FCons fmt restFl) row = case fmt of
      Var var -> case parseTypedParam var of
        Left error -> Left error
        Right (Tuple name ty) -> case parseUrlImpl restFl row of
          Left error -> Left error
          Right row' ->
            Right $ Row.insert name ty row'
      Lit lit ->
        parseUrlImpl restFl row

