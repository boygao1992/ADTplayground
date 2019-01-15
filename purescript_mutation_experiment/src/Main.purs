module Main where

import Control.Monad.ST as ST
import Data.Nullable (Nullable, null, notNull)
import Effect (Effect)
import Effect.Console (logShow)
import Prelude
import Record as R -- make a new copy each step to guarantee immutability
import Record.Builder as Builder -- mutated in place and frozen after final build step
import Record.ST as RST
import Type.Data.Symbol (SProxy(..))

main :: Effect Unit
main = do
  logShow $ ST.run do
    stRec <- RST.thaw { x : { y : 0 }, a : { b : "wenbo" } }
    x <- RST.peek (SProxy :: SProxy "x") stRec
    xRec <- RST.thaw x
    RST.peek (SProxy :: SProxy "y") xRec

  logShow $ ST.run do
    a <- RST.thaw { x : { y : 0 } }
    y <- RST.peek (SProxy :: SProxy "x") a
    RST.modify (SProxy :: SProxy "x") (_ { y = 1 }) a
    pure y
