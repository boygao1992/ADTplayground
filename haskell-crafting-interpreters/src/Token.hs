module Token where

-- [4. Scanning](http://www.craftinginterpreters.com/scanning.html)

import RIO

import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data TokenType
  -- Single-character tokens
  = LEFT_PARAN  -- (
  | RIGHT_PARAN -- )
  | LEFT_BRACE  -- {
  | RIGHT_BRACE -- }
  | COMMA       -- ,
  | DOT         -- .
  | MINUS       -- -
  | PLUS        -- +
  | SEMICOLON   -- ;
  | STAR        -- *
  | SLASH       -- / -- NOTE line comment prefix //
  -- One or two character tokens
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  -- Literals
  | IDENTIFIER
  | STRING
  | NUMBER
  -- Keywords
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
  deriving (Eq, Ord, Enum, Show)

singleCharToken :: Parser TokenType
singleCharToken
  = label "Single-character token"
  $   char '(' $> LEFT_PARAN
  <|> char ')' $> RIGHT_PARAN
  <|> char '{' $> LEFT_BRACE
  <|> char '}' $> RIGHT_BRACE
  <|> char ',' $> COMMA
  <|> char '.' $> DOT
  <|> char '-' $> MINUS
  <|> char '+' $> PLUS
  <|> char ';' $> SEMICOLON
  <|> char '*' $> STAR
  <|> char '/' $> SLASH

doubleCharToken :: Parser TokenType
doubleCharToken
  = label "One or two character token"
  $   (char '!' >> ((char '=') $> BANG_EQUAL) <|> pure BANG)
  <|> (char '=' >> ((char '=') $> EQUAL_EQUAL) <|> pure EQUAL)
  <|> (char '<' >> ((char '=') $> LESS_EQUAL) <|> pure LESS)
  <|> (char '>' >> ((char '=') $> GREATER_EQUAL) <|> pure GREATER)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser Text
symbol' = L.symbol' sc

data Token a = Token
  { _type    :: !TokenType
  , _lexeme  :: !String
  , _literal :: !a
  , _line    :: !Int
  }
