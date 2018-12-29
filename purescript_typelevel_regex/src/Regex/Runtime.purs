module Regex.Runtime where

import Prelude

import Data.Array (reverse, uncons, cons) as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
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
  | CharNumFixed Char Int -- TODO Char Int
  | CharNumMaybe Char
  | CharNumPositive Char
  | CharStar Char
  | AnyChar
  | AnyStar
derive instance genericPattern :: Generic Pattern _
instance showPattern :: Show Pattern where
  show = genericShow

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
    parseInductionStep AnyToken restTokens = do
      restPattern <- parseBaseCase restTokens
      pure $ AnyChar `A.cons` restPattern
    parseInductionStep StarToken restTokens = case A.uncons restTokens of
      Nothing -> Left "incomplete Star"
      Just { head : h, tail : t } -> case h of
        LitToken c -> do
          restPattern <- parseBaseCase t
          pure $ CharStar c `A.cons` restPattern
        AnyToken -> do
          restPattern <- parseBaseCase t
          pure $ AnyStar `A.cons` restPattern
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
        _ -> Left "invalid token when parsing a single char"

-- recognize :: Array Pattern -> String -> Boolean
-- recognize patterns str =
--   let
--     chars = String.toCharArray str
--   in
--     case (A.uncons patterns), (A.uncons chars) of
--       Nothing, Just _ -> false
--       Just _, Nothing -> false
--       Nothing, Nothing -> true
--       Just { head: p_h, tail: p_t }, Just { head: s_h, tail: s_t } ->
--         recognizeInductionStep p_h p_t s_h s_t

-- recognizeInductionStep :: Pattern -> Array Pattern -> Char -> Array Char -> Boolean
-- recognizeInductionStep p_h p_t s_h s_t = case p_h of
--   Lit c ->
--     (c == s_h) && recognizeBaseCase p_t s_t
--   CharNumFixed


-- recognizeBaseCase :: Array Pattern -> Array Char -> Boolean
-- recognizeBaseCase _ _ = true
