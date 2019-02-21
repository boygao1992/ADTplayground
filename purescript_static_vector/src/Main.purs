module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Type.Data.Symbol.Num.Int (toInt)
import Type.Data.Symbol (SProxy (..))

main :: Effect Unit
main = do
  logShow $ toInt (SProxy :: SProxy "-0080101")
