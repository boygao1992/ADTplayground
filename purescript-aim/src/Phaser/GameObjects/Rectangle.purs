module Phaser.GameObjects.Rectangle
  ( Rectangle
  , toShape
  ) where

import Phaser.GameObjects.Shape as Phaser.GameObjects.Shape
import Unsafe.Coerce as Unsafe.Coerce

---------------------------
-- Phaser.GameObjects.Rectangle
---------------------------
foreign import data Rectangle :: Type

toShape :: Rectangle -> Phaser.GameObjects.Shape.Shape
toShape = Unsafe.Coerce.unsafeCoerce
