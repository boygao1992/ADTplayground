module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Class as State
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)

newtype App a = App ((StateT Int Effect) a)
derive newtype instance functorApp :: Functor App
derive newtype instance applyApp :: Apply App
derive newtype instance bindApp :: Bind App
derive newtype instance applicativeApp :: Applicative App
derive newtype instance monadApp :: Monad App
derive newtype instance monadStateApp :: MonadState Int App

split :: StateT String (Either String) String
split = do
  s <- State.get
  case s of
    "" -> lift $ Left "Empty string"
    _  -> do
      State.put "valid"
      pure "return"

main :: Effect Unit
main = do
  log $ show $ runStateT split ""
  log $ show $ runStateT split "one"
