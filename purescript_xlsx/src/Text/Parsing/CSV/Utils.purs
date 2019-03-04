module Text.Parsing.CSV.Utils where

import Prelude

import Data.Array as A
import Data.HObject (HObject, (-=), hObj)
import Data.List (List(..), zip)
import Data.Tuple (Tuple(..))
import Text.Parsing.CSV (P, defaultParsers)

makeHObject :: P (List (List String)) -> P (List (HObject String))
makeHObject file = do
  f <- file
  let ll = case f of
            Nil -> Nil
            Cons header rows -> mkRow header <$> rows
  pure do
    l <- ll
    pure $ hObj <<< A.fromFoldable <<< map (\(Tuple columnName value) -> columnName -= value) $ l
  where
    mkRow header row' = zip header row'

hObject :: P (List (HObject String))
hObject = makeHObject defaultParsers.file
