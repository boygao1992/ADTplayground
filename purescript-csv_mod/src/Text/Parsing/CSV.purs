module Text.Parsing.CSV where

import Prelude

import Control.Alt ((<|>))
import Data.Array (some) as Array
import Data.List (List(..), zip)
import Data.Map as M
import Data.Profunctor (dimap)
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton) as String
import Text.Parsing.Parser (Parser, ParserT)
import Text.Parsing.Parser.Combinators (sepEndBy, sepBy1, between) as Parse
import Text.Parsing.Parser.String (char, eof, noneOf, satisfy, string) as Parse
import Text.Parsing.Parser.String (class StringLike)

type P a = Parser String a

type Parsers a =
  { quoted :: (P a -> P a)
  , chars :: P String
  , qchars :: P String
  , field :: P String
  , row :: P (List String)
  , file :: P (List (List String))
  , fileHeaded :: P (List (M.Map String String))
  }

makeQuoted :: forall a. Char -> (P a -> P a)
makeQuoted q = Parse.between (Parse.char q) (Parse.char q)

makeChars :: String -> P String
makeChars =
  dimap
    String.toCharArray
    (map String.fromCharArray)
    ( \str -> Array.some ( Parse.noneOf str) )

makeQchars :: Char -> P String
makeQchars c =
  String.fromCharArray
  <$> Array.some (qchar <|> escapedQuote)

  where
    qchar :: forall s m. StringLike s => Monad m => ParserT s m Char
    qchar = Parse.satisfy (_ /= c)

    escapedQuote :: P Char
    escapedQuote = (Parse.string $ String.fromCharArray [c, c]) $> c

makeField :: (P String -> P String) -> P String -> P String -> P String
makeField qoutes qoutedChars purechars =
  qoutes qoutedChars <|> purechars <|> Parse.string ""

makeRow :: String -> P String -> P (List String)
makeRow s f = f `Parse.sepBy1` Parse.string s

makeFile :: String -> P (List String) -> P (List (List String))
makeFile s r = r `Parse.sepEndBy` Parse.string s <* Parse.eof

makeFileHeaded :: P (List (List String)) -> P (List (M.Map String String))
makeFileHeaded file = do
  f <- file
  pure $ case f of
    Nil -> Nil
    Cons header rows -> mkRow header <$> rows
  where
    mkRow header row' = M.fromFoldable $ zip header row'

makeParsers :: Char -> String -> String -> Parsers String
makeParsers quote seperator eol = do
  let seperator' = String.toCharArray seperator
  let eol' = String.toCharArray eol
  let quoted' = makeQuoted quote
  let chars' = makeChars $ (String.singleton quote) <> seperator <> eol
  let qchars' = makeQchars quote
  let field' = makeField quoted' qchars' chars'
  let row' = makeRow seperator field'
  let file' = makeFile eol row'
  let fileHeaded' = makeFileHeaded file'
  {
    quoted: quoted',
    chars: chars',
    qchars: qchars',
    field: field',
    row: row',
    file: file',
    fileHeaded: fileHeaded'
  }

defaultParsers :: Parsers String
defaultParsers = makeParsers '"' "," "\n"
