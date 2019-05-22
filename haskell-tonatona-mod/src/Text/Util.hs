module Text.Util where

import RIO
import RIO.Text as Text (toLower, toUpper, splitAt, append)

-- | Make the first letter of a Text upper case.
capitalize :: Text -> Text
capitalize word =
  let (head, rest) = Text.splitAt 1 word
  in Text.toUpper head `Text.append` rest

-- | Make the first letter of a Text lower case.
uncapitalize :: Text -> Text
uncapitalize word =
  let (head, rest) = Text.splitAt 1 word
  in Text.toLower head `Text.append` rest
