module Data.String.Read where

import Prelude

import Data.Maybe (Maybe (..), fromMaybe)
import Data.Int as Int
import Data.Number as Number

class Read a where
  read :: String -> Maybe a

instance readInt :: Read Int where
  read = Int.fromString

instance readNumber :: Read Number where
  read = Number.fromString

instance readString :: Read String where
  read = Just

instance readBoolean :: Read Boolean where
  read "true" = Just true
  read "false" = Just false
  read _ =  Nothing

class Empty a where
  empty :: a

instance emptyInt :: Empty Int where
  empty = 0

instance emptyNumber :: Empty Number where
  empty = 0.0

instance emptyString :: Empty String where
  empty = ""

instance emptyBoolean :: Empty Boolean where
  empty = false

fill :: forall a. Read a => Empty a => String -> a
fill = fromMaybe empty <<< read
