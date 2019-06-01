module PapaParse where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Data.Tuple (Tuple)

-- | Type

type ParseError =
  { code :: String
  , index :: Int
  , message :: String
  , row :: Int
  , type :: String
  }

type ParseMeta =
  { aborted :: Boolean
  , cursor :: Int
  , delimiter :: Char
  , linebreak :: Char
  , truncated :: Boolean
  }

type ParseResult =
  { data :: Array (Array String)
  , errors :: Array ParseError
  , meta :: ParseMeta
  }
-- | FFI

foreign import _parse :: EffectFn1 String ParseResult

-- | API

parseRaw :: String -> Effect ParseResult
parseRaw str = runEffectFn1 _parse str

parseFile :: String -> Effect (Either (Array ParseError) (Array (Array String)))
parseFile
  = parseRaw
  >>> map \result ->
      if Array.null result.errors
      then Right result.data
      else Left result.errors

parseFileHeadedList
  :: String
  -> Effect (Either (Array ParseError) (Array (Array (Tuple String String))))
parseFileHeadedList = map (bind <@> _parseFileHeadedList) <<< parseFile
  where
    _parseFileHeadedList
      :: Array (Array String)
      -> Either (Array ParseError) (Array (Array (Tuple String String)))
    _parseFileHeadedList file = case Array.uncons file of
      Just { head, tail } -> Right $ Array.zip head <$> tail
      Nothing -> Left []

