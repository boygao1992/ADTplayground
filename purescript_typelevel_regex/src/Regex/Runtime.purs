module Regex.Runtime where

import Prelude

import Data.Array (reverse, uncons, cons) as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Utils (parseInt)

case1StartEndToken = "^abc$" :: String --- "abc"
case2Number = "a{3}" :: String -- "aaa"
case3QuestionMark = "a?" :: String -- zero or one
case4Positive = "a+" :: String -- one or more
case5Any = "." :: String -- any char
case6CharStar = "a*" :: String -- zero or more
case7AnyCharStar = ".*" :: String -- ignore any number of consecutive chars, exhaustive search

edgecase1 = "a*a*" :: String -- HACK greedy, first "a*" takes everything

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
  -- = Lit Char -- equivalent to (CharNumFixed Char 1)
  -- | AnyChar
  = CharNumFixed Char Int
  -- | AnyCharNumFixed Int
  | CharNumMaybe Char
  -- | AnyCharNumMaybe
  | CharNumPositive Char
  -- | AnyCharNumPositive
  | CharStar Char
  -- | AnyCharStar
derive instance genericPattern :: Generic Pattern _
instance showPattern :: Show Pattern where
  show = genericShow
instance eqPattern :: Eq Pattern where
  eq = genericEq

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
      pure $ (CharNumFixed c 1) `A.cons` restPatterns
    parseInductionStep AnyToken restTokens = do
      restPattern <- parseBaseCase restTokens
      pure $ (CharNumFixed '.' 1) `A.cons` restPattern -- HACK '.' to AnyToken
    parseInductionStep NumFixedRightToken rest1 = do
      intResult <- parseCharNumFixedInt rest1
      charResult <- parseSingleChar intResult.rest
      restPattern <- parseBaseCase charResult.rest -- recursive call
      case parseInt 10 intResult.int of
        Nothing -> Left "invalid tokens in CharNumFixed: expect base10 digits in between `{` and `}`"
        Just int -> Right $ CharNumFixed charResult.char int `A.cons` restPattern
    parseInductionStep MaybeToken restTokens = do
      charResult <- parseSingleChar restTokens
      restPattern <- parseBaseCase charResult.rest
      pure $ CharNumMaybe charResult.char `A.cons` restPattern
    parseInductionStep NumPositiveToken restTokens = do
      charResult <- parseSingleChar restTokens
      restPattern <- parseBaseCase charResult.rest
      pure $ CharNumPositive charResult.char `A.cons` restPattern
    parseInductionStep StarToken restTokens = case A.uncons restTokens of
      Nothing -> Left "incomplete Star"
      Just { head : h, tail : t } -> case h of
        LitToken c -> do
          restPattern <- parseBaseCase t
          pure $ CharStar c `A.cons` restPattern
        AnyToken -> do
          restPattern <- parseBaseCase t
          pure $ (CharStar '.') `A.cons` restPattern
        _ -> Left "invalid token, cannot be embellished by Star"
    parseInductionStep NumFixedLeftToken _ = Left "incomplete CharNumFixed"


    parseCharNumFixedInt :: Array Token -> Either Error { int :: String, rest :: Array Token }
    parseCharNumFixedInt = A.uncons >>> case _ of
      Nothing -> Left "incomplete CharNumFixed"
      Just { head: h, tail: t } ->
        parseCharNumFixedIntImpl h t

    parseCharNumFixedIntImpl :: Token -> Array Token -> Either Error { int :: String, rest :: Array Token }
    parseCharNumFixedIntImpl h t = case A.uncons t of
      Nothing -> -- need another 2 chars ("{1") to complete the pattern
        Left "incomplete CharNumFixedInt"
      Just { head: t_h, tail: t_t } -> case h, t_h of
        (LitToken d1), (LitToken d2) -> do -- induction step
          result <- parseCharNumFixedIntImpl t_h t_t
          pure $ { int : (String.singleton d1) <> result.int, rest : result.rest }
        (LitToken d), NumFixedLeftToken -> -- base case
          pure $ { int : (String.singleton d), rest : t_t }
        _, _ -> Left "invalid token when parseing CharNumFixedInt"

    parseSingleChar :: Array Token -> Either Error { char :: Char, rest :: Array Token}
    parseSingleChar = A.uncons >>> case _ of
      Nothing -> Left "EOL"
      Just { head : h, tail : t } -> case h of
        LitToken c -> pure { char : c, rest : t }
        AnyToken -> pure { char : '.', rest : t } -- HACK '.' to AnyToken
        _ -> Left "invalid token when parsing a single char"

recognize :: Array Pattern -> String -> Boolean
recognize patterns str = recognizeBaseCase patterns (String.toCharArray str)

recognizeBaseCase :: Array Pattern -> Array Char -> Boolean
recognizeBaseCase p s = case A.uncons p of
  Nothing ->
    if s == []
    then
      true
    else
      false
  Just { head: p_h, tail: p_t } ->
    recognizeInductionStep p_h p_t s

recognizeInductionStep :: Pattern -> Array Pattern -> Array Char -> Boolean
recognizeInductionStep p_h p_t s = case p_h of
  CharNumFixed c n ->
    recognizeCharNumFixedBaseCase c n p_t s
  CharNumPositive c ->
    recognizeCharNumPositiveBaseCase c p_t s
  CharNumMaybe c ->
    recognizeCharNumMaybeBaseCase c p_t s
  CharStar c ->
    recognizeCharStar c p_t s

recognizeCharNumFixedBaseCase :: Char -> Int -> Array Pattern -> Array Char -> Boolean
recognizeCharNumFixedBaseCase = go
  where
    go :: Char -> Int -> Array Pattern -> Array Char -> Boolean
    go c n p_t s
      | n < 0 = false
      | n == 0 = recognizeBaseCase p_t s
      | otherwise = case A.uncons s of -- n > 0
        Nothing ->
          false
        Just { head : s_h, tail : s_t } ->
          recognizeCharNumFixedInductionStep c n p_t s_h s_t

recognizeCharNumFixedInductionStep :: Char -> Int -> Array Pattern -> Char -> Array Char -> Boolean
recognizeCharNumFixedInductionStep '.' n p_t _ s_t =
  recognizeCharNumFixedBaseCase '.' (n - 1) p_t s_t
recognizeCharNumFixedInductionStep c n p_t s_h s_t =
  if c == s_h
  then
    recognizeCharNumFixedBaseCase c (n - 1) p_t s_t
  else
    false

recognizeCharNumMaybeBaseCase :: Char -> Array Pattern -> Array Char -> Boolean
recognizeCharNumMaybeBaseCase c p_t s =
  if recognizeBaseCase p_t s
  then
    true
  else
    case A.uncons s of
      Nothing ->
        false
      Just { head : s_h, tail : s_t } ->
        recognizeCharNumMaybeInductionStep c p_t s_h s_t

recognizeCharNumMaybeInductionStep :: Char -> Array Pattern -> Char -> Array Char -> Boolean
recognizeCharNumMaybeInductionStep '.' p_t s_h s_t = -- AnyCharNumMaybe
    recognizeBaseCase p_t s_t
recognizeCharNumMaybeInductionStep c p_t s_h s_t = -- CharNumMaybe
  if c == s_h
  then
    recognizeBaseCase p_t s_t
  else
    false

recognizeCharNumPositiveBaseCase :: Char -> Array Pattern -> Array Char -> Boolean
recognizeCharNumPositiveBaseCase c p_t s = case A.uncons s of
  Nothing ->
    recognizeBaseCase p_t s
  Just { head: s_h, tail: s_t } ->
    recognizeCharNumPositiveInductionStep c p_t s_h s_t

recognizeCharNumPositiveInductionStep :: Char -> Array Pattern -> Char -> Array Char -> Boolean
recognizeCharNumPositiveInductionStep '.' p_t s_h s_t = -- AnyCharNumPositive
  if recognizeBaseCase p_t s_t
  then
    true
  else
    recognizeCharNumPositiveBaseCase '.' p_t s_t
recognizeCharNumPositiveInductionStep c p_t s_h s_t = -- CharNumPositive
  if c == s_h
  then
    if recognizeBaseCase p_t s_t
    then
      true
    else
      recognizeCharNumPositiveBaseCase c p_t s_t
  else
    false


recognizeCharStar :: Char -> Array Pattern -> Array Char -> Boolean
recognizeCharStar c p_t s =
  if recognizeBaseCase p_t s
  then
    true
  else
    recognizeCharNumPositiveBaseCase c p_t s
