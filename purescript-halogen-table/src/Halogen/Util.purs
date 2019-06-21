module Halogen.Util where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H

-- Util
debug
  :: forall state action slots output m
  . MonadAff m => String -> H.HalogenM state action slots output m Unit
debug = H.liftEffect <<< Console.log

debugShow
  :: forall state action slots output m a
  . Show a
  => MonadAff m
  => a -> H.HalogenM state action slots output m Unit
debugShow = H.liftEffect <<< Console.logShow
