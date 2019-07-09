module MySQL.Formatter
( timeStampFormatter
) where

import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.List as List

timeStampFormatter :: Formatter
timeStampFormatter =
  List.fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  , Placeholder " "
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  ]
