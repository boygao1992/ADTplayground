module Phaser.GameObjects.Line
  ( Line
  , setLineWidth
  , toShape
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.Shape as Phaser.GameObjects.Shape
import Unsafe.Coerce as Unsafe.Coerce

--------------------------
-- Phaser.GameObjects.Line
--------------------------
foreign import data Line :: Type

toShape :: Line -> Phaser.GameObjects.Shape.Shape
toShape = Unsafe.Coerce.unsafeCoerce

foreign import _setLineWidth ::
  Effect.Uncurried.EffectFn1
    { line :: Line
    , startWidth :: Number
    , endWidth :: Number
    }
    Unit

setLineWidth :: Line -> Number -> Effect Unit
setLineWidth line width =
  Effect.Uncurried.runEffectFn1
    _setLineWidth
    { line
    , startWidth: width
    , endWidth: width
    }
