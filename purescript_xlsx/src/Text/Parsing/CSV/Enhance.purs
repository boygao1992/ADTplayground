module Text.Parsing.CSV.Enhance where

import Prelude

import Text.Parsing.CSV (P, makeChars, makeField, makeFile, makeFileHeaded, makeQchars, makeQuoted, makeRow)
import Data.String.CodeUnits (singleton)
import Data.List (List(..), zip, groupBy)
import Data.List.NonEmpty as NEL
import Data.Profunctor.Strong ((&&&))
import Data.Function (on)
import Data.Tuple (Tuple, fst, snd)
import Data.Map as M

type Parsers' a =
  { quoted :: (P a -> P a)
  , chars :: P String
  , qchars :: P String
  , field :: P String
  , row :: P (List String)
  , file :: P (List (List String))
  , fileHeaded :: P (List (M.Map String String))
  , fileHeadedList :: P (List (List (Tuple String String)))
  , fileHeadedDup :: P (List (M.Map String (NEL.NonEmptyList String)))
  }

makeFileHeadedList :: P (List (List String)) -> P (List (List (Tuple String String)))
makeFileHeadedList file = do
  f <- file
  pure $ case f of
    Nil -> Nil
    Cons header rows -> mkRow header <$> rows
  where
    mkRow :: List String -> List String -> List (Tuple String String)
    mkRow header row = zip header row

makeFileHeadedDup :: P (List (List (Tuple String String))) -> P (List (M.Map String (NEL.NonEmptyList String)))
makeFileHeadedDup table = do
  t <- table
  pure $ M.fromFoldable
    <<< map ((fst <<< NEL.head) &&& (map snd))
    <<< groupBy (on (==) fst)
    <$> t

-- TODO rebuild on top of makeParsers
makeParsers' :: Char -> String -> String -> Parsers' String
makeParsers' quote seperator eol = do
  let quoted' = makeQuoted $ singleton quote
  let chars' = makeChars $ (singleton quote) <> seperator <> eol
  let qchars' = makeQchars quote
  let field' = makeField quoted' qchars' chars'
  let row' = makeRow seperator field'
  let file' = makeFile eol row'
  let fileHeaded' = makeFileHeaded file'
  let fileHeadedList' = makeFileHeadedList file'
  let fileHeadedDup' = makeFileHeadedDup fileHeadedList'
  { quoted: quoted'
  , chars: chars'
  , qchars: qchars'
  , field: field'
  , row: row'
  , file: file'
  , fileHeaded: fileHeaded'
  , fileHeadedList: fileHeadedList'
  , fileHeadedDup: fileHeadedDup'
  }

defaultParsers' :: Parsers' String
defaultParsers' = makeParsers' '"' "," "\n"
