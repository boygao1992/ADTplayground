module Text.Parsing.CSV where

import Prelude

import Control.Alt ((<|>))
import Data.Array (some) as Array
import Data.Function (on)
import Data.List (List(..))
import Data.List (groupBy, zip) as List
import Data.List.NonEmpty as NEL
import Data.Map as M
import Data.Profunctor (dimap)
import Data.Profunctor.Strong ((&&&))
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton) as String
import Data.Tuple (Tuple, fst, snd)
import Text.Parsing.Parser (Parser, ParserT)
import Text.Parsing.Parser.Combinators (sepEndBy, sepBy1, between) as PS
import Text.Parsing.Parser.String (char, eof, noneOf, satisfy, string) as PS
import Text.Parsing.Parser.String (class StringLike)

type P a = Parser String a

type Parsers a =
  { quoted :: (P a -> P a)
  , chars :: P String
  , qchars :: P String
  , field :: P String
  , row :: P (List String)
  , file :: P (List (List String))
  , fileHeadedList :: P (List (List (Tuple String String)))
  , fileHeadedMap :: P (List (M.Map String String))
  , fileHeadedDup :: P (List (M.Map String (NEL.NonEmptyList String)))
  }

makeQuoted :: forall a. Char -> (P a -> P a)
makeQuoted q = PS.between (PS.char q) (PS.char q)

makeChars :: String -> P String
makeChars =
  dimap
    String.toCharArray
    (map String.fromCharArray)
    ( \str -> Array.some ( PS.noneOf str) )

makeQchars :: Char -> P String
makeQchars c =
      qchar <|> escapedQuote
    # Array.some
  <#> String.fromCharArray

  where
    qchar :: forall s m. StringLike s => Monad m => ParserT s m Char
    qchar = PS.satisfy (_ /= c)

    escapedQuote :: P Char
    escapedQuote = (PS.string $ String.fromCharArray [c, c]) $> c

makeField :: (P String -> P String) -> P String -> P String -> P String
makeField qoutes qoutedChars purechars =
  qoutes qoutedChars <|> purechars <|> PS.string ""

makeRow :: String -> P String -> P (List String)
makeRow s f = f `PS.sepBy1` PS.string s

makeFile
  :: String
  -> P (List String) -> P (List (List String))
makeFile s r = r `PS.sepEndBy` PS.string s <* PS.eof

makeFileHeadedList
  :: P (List (List String))
  -> P (List (List (Tuple String String)))
makeFileHeadedList file = do
  f <- file
  pure $ case f of
    Nil -> Nil
    Cons header rows -> List.zip header <$> rows

makeFileHeadedMap
  :: P (List (List (Tuple String String)))
  -> P (List (M.Map String String))
makeFileHeadedMap = map $ map M.fromFoldable

makeFileHeadedDup
  :: P (List (List (Tuple String String)))
  -> P (List (M.Map String (NEL.NonEmptyList String)))
makeFileHeadedDup = map $ map $
        M.fromFoldable
    <<< map ((fst <<< NEL.head) &&& (map snd))
    <<< List.groupBy (on (==) fst)

makeParsers :: Char -> String -> String -> Parsers String
makeParsers quote seperator eol =
  let
    quoted = makeQuoted quote
    chars = makeChars $ (String.singleton quote) <> seperator <> eol
    qchars = makeQchars quote
    field = makeField quoted qchars chars
    row = makeRow seperator field
    file = makeFile eol row
    fileHeadedList = makeFileHeadedList file
    fileHeadedMap = makeFileHeadedMap fileHeadedList
    fileHeadedDup = makeFileHeadedDup fileHeadedList
  in
    { quoted
    , chars
    , qchars
    , field
    , row
    , file
    , fileHeadedList
    , fileHeadedMap
    , fileHeadedDup
    }

defaultParsers :: Parsers String
defaultParsers = makeParsers '"' "," "\n"
