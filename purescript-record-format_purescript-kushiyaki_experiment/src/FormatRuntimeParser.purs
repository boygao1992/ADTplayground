module Record.Format.RuntimeParser where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, uncons) as String

type Symbol = String
data Fmt
  = Var Symbol
  | Lit Symbol

data FList
  = FNil
  | FCons Fmt FList

-- type-level Char doesn't have its distinct kind
-- use IsSymbol type class to refine Symbol into Symbol-Char

parse :: Symbol -> FList
parse = String.uncons >>> case _ of
  Nothing -> FNil
  Just {head, tail} -> parseLit head tail

parseLit :: Char -> Symbol -> FList
parseLit head tail = case tail of
  "" -> FCons (Lit (String.singleton head)) FNil
  "{" -> 
