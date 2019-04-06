module Main where

import Prelude

import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Console (logShow)
import Text.Parsing.Parser (ParseError, ParseState(..), ParserT(..), runParser) as PS
import Text.Parsing.Parser.String (char) as PS
import Text.Parsing.Parser.Pos (Position(..)) as PS
import Data.Array as Array
import Data.String.CodeUnits as String
import Control.Monad.State.Trans (StateT(..), runStateT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Newtype (unwrap)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

runParserTWithState
  :: forall m i a
   . Monad m
  => PS.ParseState i
  -> PS.ParserT i m a
  -> m (Tuple (Either PS.ParseError a) (PS.ParseState i))
runParserTWithState s = (runStateT <@> s) <<< runExceptT <<< unwrap

alt'
  :: forall a i m
   . Monad m
  => PS.ParserT i m a
  -> PS.ParserT i m a
  -> PS.ParserT i m a
alt' p1 p2 = PS.ParserT <<< ExceptT <<< StateT $
  \s@(PS.ParseState i (PS.Position p) c) -> do
    Tuple e s'@(PS.ParseState i' (PS.Position p') c') <-
      runParserTWithState (PS.ParseState i (PS.Position p) false) p1
    case e of
      Left err
        | not c' -> runParserTWithState s p2
      _ -> pure (Tuple e s')

main :: Effect Unit
main = do
  logShow
      $ PS.runParser "fafafbc"
      $ String.fromCharArray
    <$> Array.some (PS.char 'a' <|> PS.char 'f')
