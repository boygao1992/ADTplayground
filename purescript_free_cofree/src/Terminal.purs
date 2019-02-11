module Terminal where

import Prelude

import Control.Monad.Free (Free, liftF)

-- [Haskell Live-Coding, Session 2.1, Q&A](https://www.twitch.tv/videos/258496567?filter=all&sort=time)

-- | Two basic types of Queries
type Action f = Unit -> f Unit
type Request f a = (a -> a) -> f a

action :: forall f. Action f -> f Unit
action act = act unit

request :: forall f a. Request f a -> f a
request req = req identity

-- | Free DSL
data TerminalF a
  = GetChr (Char -> a)
  | PutChr Char a
derive instance functorTerminalF :: Functor TerminalF

newtype Terminal a = Terminal (Free TerminalF a)
derive newtype instance functorTerminal :: Functor Terminal
derive newtype instance applyTerminal :: Apply Terminal
derive newtype instance applicativeTerminal :: Applicative Terminal
derive newtype instance bindTerminal :: Bind Terminal
derive newtype instance monadTerminal :: Monad Terminal

class Monad m <= MonadTerminal m where
  getChr :: m Char
  putChr :: Char -> m Unit

instance monadTerminalTerminal:: MonadTerminal Terminal where
  getChr = Terminal <<< liftF $ request GetChr
  putChr x = Terminal <<< liftF $ action (PutChr x)

{-
instance monadTerminalEffect :: MonadTerminal Effect where
  getChr = getChar
  putChr = putChar
-}

{- interpreter :: Terminal ~> Effect -}
