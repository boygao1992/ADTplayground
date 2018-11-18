module Main where

import Prelude

import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.List (fromFoldable) as List
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Now (nowDateTime) as Now

main :: Effect Unit
main = do
  dateTime <- Now.nowDateTime
  logShow $ format formatter dateTime

  where
    formatter :: Formatter
    formatter = List.fromFoldable
      [ MinutesTwoDigits
      , Placeholder ":"
      , SecondsTwoDigits
      , Placeholder ":"
      , MillisecondsTwoDigits
      ]
