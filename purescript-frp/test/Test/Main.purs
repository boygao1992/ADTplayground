module Test.Main where

import Prelude
import Effect (Effect)
import Test.Event as Test.Event
import Test.Unit.Main as Test.Unit.Main

main :: Effect Unit
main =
  Test.Unit.Main.runTest do
    Test.Event.suite
