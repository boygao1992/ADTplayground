module Record.Format.RuntimeParser where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, uncons) as Symbol
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

type Symbol = String
data Fmt
  = Var Symbol
  | Lit Symbol
derive instance genericFmt :: Generic Fmt _
instance showFmt :: Show Fmt where
  show = genericShow

data FList
  = FNil
  | FCons Fmt FList
instance showFList :: Show FList where
  show FNil = "FNil"
  show (FCons fmt flist) = (show fmt) <> " : " <> show flist

-- type-level Char doesn't have its distinct kind
-- use IsSymbol type class to refine Symbol into Symbol-Char

parse :: Symbol -> FList
parse = Symbol.uncons >>> case _ of
  Nothing -> FNil
  Just { head : h, tail : t } ->
    parseLit (Symbol.singleton h) t

  where
    parseLit :: Symbol -> Symbol -> FList
    parseLit h t = case Symbol.uncons t of
      Nothing -> -- t == ""
        FCons (Lit h) FNil
      Just {head : t_h, tail : t_t } ->
        case h of
          "{" ->
            let
              Tuple var rest = parseVar (Symbol.singleton t_h) t_t
              restFl = parse rest
            in
              case var of
                Var match ->
                  FCons (Lit "") (FCons (Var match) restFl)
                _ ->
                  FNil -- Error
          _ ->
            let
              fl = parse t
              Tuple restLit restFl = case fl of
                FCons ftm restFl ->
                  case ftm of
                    Lit restLit -> Tuple (h <> restLit) restFl
                    _ -> Tuple "" FNil -- Error
                _ -> Tuple "" FNil -- Error
            in
              FCons (Lit restLit) restFl
    parseVar :: Symbol -> Symbol -> Tuple Fmt Symbol
    parseVar h t = case h of
      "" -> Tuple (Var "") ""
      "}" -> Tuple (Var "") t
      _ -> case Symbol.uncons t of
        Nothing -> Tuple (Var h) ""
        Just { head : t_h, tail : t_t } ->
          let
            Tuple fmt rest = parseVar (Symbol.singleton t_h) t_t
          in
            case fmt of
              Var restVar ->
                Tuple (Var (h <> restVar)) rest
              _ ->
                Tuple (Var "") rest -- Error

type Type_ = String

data Row_
  = RowNil
  | RowCons Symbol Type_ Row_
