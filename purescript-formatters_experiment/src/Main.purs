module Main where

import Prelude

import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.Formatter.Number as Number
import Data.List (fromFoldable) as List
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Now (nowDateTime) as Now

main :: Effect Unit
main = do
  dateTime <- Now.nowDateTime
  log$ format formatter dateTime

  log $ Number.printFormatter numberFormatter
  log $ Number.format numberFormatter 1234567.8901

  where
    formatter :: Formatter
    formatter = List.fromFoldable
      [ MinutesTwoDigits
      , Placeholder ":"
      , SecondsTwoDigits
      , Placeholder ":"
      , MillisecondsTwoDigits
      ]

    numberFormatter :: Number.Formatter
    numberFormatter = Number.Formatter
      { comma: true
      , before: 10
      , after: 3
      , abbreviations: false
      , sign: false
      }
