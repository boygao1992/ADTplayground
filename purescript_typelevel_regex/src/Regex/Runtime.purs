module Regex.Runtime where

import Prelude

import Data.Array (reverse, uncons, cons) as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String

case1StartEndToken = "^abc$" :: String --- "abc"
case2Number = "a{3}" :: String -- "aaa"
case3QuestionMark = "a?" :: String -- zero or one
case4Positive = "a+" :: String -- one or more
case5Any = "." :: String -- any char
case6CharStar = "a*" :: String -- zero or more
case7AnyStar = ".*" :: String -- ignore any number of consecutive chars, exhaustive search

type Error = String

data Token
  = LitToken Char
  | NumFixedLeftToken
  | NumFixedRightToken
  | MaybeToken
  | NumPositiveToken
  | AnyToken
  | StarToken
derive instance genericToken :: Generic Token _
instance showToken :: Show Token where
  show = genericShow

toToken :: Char -> Token
toToken '{' = NumFixedLeftToken
toToken '}' = NumFixedRightToken
toToken '?' = MaybeToken
toToken '+' = NumPositiveToken
toToken '.' = AnyToken
toToken '*' = StarToken
toToken c = LitToken c

tokenize :: String -> Array Token
tokenize = map toToken <<< String.toCharArray

data Pattern
  = Lit Char
  | CharNumFixed Char Int
  | CharNumMaybe Char
  | CharNumPositive Char
  | CharStar Char
  | AnyChar
  | AnyStar


parse :: Array Token -> Either Error (Array Pattern)
parse = map A.reverse <<< parseBaseCase <<< A.reverse
  where
    parseBaseCase :: Array Token -> Either Error (Array Pattern)
    parseBaseCase = A.uncons >>> case _ of
      Nothing -> pure []
      Just { head: h, tail: t } -> parseInductionStep h t

    parseInductionStep :: Token -> Array Token -> Either Error (Array Pattern)
    parseInductionStep (LitToken c) restTokens = do
      restPatterns <- parseBaseCase restTokens
      pure $ (Lit c) `A.cons` restPatterns
    parseInductionStep NumFixedRightToken restTokens = do
      result <- parseCharNumFixed restTokens
      restRestTokens <- parseBaseCase result.tokens
      pure $ result.pattern `A.cons` restRestTokens
    parseInductionStep _ _ = Left ""


    parseCharNumFixed :: Array Token -> Either Error { pattern :: Pattern, tokens :: Array Token }
    parseCharNumFixed _ = Left ""
