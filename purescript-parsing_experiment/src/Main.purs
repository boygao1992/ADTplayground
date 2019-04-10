module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
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
import Control.Lazy (class Lazy, defer)

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

-- strictly > 0, if = 0 then throw
some
  :: forall f t a
   . Alternative f
   => Lazy (f (t a))
   => Applicative t
   => Monoid (t a)
   => f a -> f (t a)
some v = ((<>) <<< pure) <$> v <*> defer (\_ -> many v)

-- >= 0, if = 0, provide monoidal unit as default
many
  :: forall f t a
   . Alternative f
  => Lazy (f (t a))
  => Applicative t
  => Monoid (t a)
  => f a -> f (t a)
many v = some v <|> pure mempty

someRec
  :: forall f t a
   . MonadRec f
  => Alternative f
  => Applicative t
  => Monoid (t a)
  => f a -> f (t a)
someRec v = ((<>) <<< pure) <$> v <*> manyRec v

manyRec
  :: forall f t a
   . MonadRec f
  => Alternative f
  => Applicative t
  => Monoid (t a)
  => f a -> f (t a)
manyRec v = tailRecM go mempty
  where
    go :: t a -> f (Step (t a) (t a))
    go acc
      -- NOTE right append for List is expensive (O(n))
      -- original implementation of `manyRec` for List uses left append (`Cons`, O(1)) and applies `reverse` on termination (`Done`)
      -- NOTE both left and right append for Array is expensive (O(n)) because it makes a new copy to preserve immutability
      -- NOTE Sequence has better concatenation performance (O(log(min(n1,n2)))
      = (Loop <<< (acc <> _) <<< pure ) <$> v
      <|> pure (Done acc)

main :: Effect Unit
main = do
  logShow
      $ PS.runParser "bc"
      $ String.fromCharArray
 -- <$> Array.many (PS.char 'a' <|> PS.char 'f')
    <$> some (PS.char 'a' `alt'` PS.char 'f')
