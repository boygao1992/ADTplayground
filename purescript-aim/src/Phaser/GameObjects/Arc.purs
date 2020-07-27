module Phaser.GameObjects.Arc
  ( Arc
  , toShape
  ) where

import Phaser.GameObjects.Shape as Phaser.GameObjects.Shape
import Unsafe.Coerce as Unsafe.Coerce

-------------------------
-- Phaser.GameObjects.Arc
-------------------------
foreign import data Arc :: Type

toShape :: Arc -> Phaser.GameObjects.Shape.Shape
toShape = Unsafe.Coerce.unsafeCoerce
