module Phaser.Class.GameObjects.Shape
  ( setAngle
  , setIsFilled
  , setIsStroked
  , setFillStyle
  , setStrokeStyle
  , class Shape
  , toShape
  ) where

import Prelude
import Effect (Effect)
import Phaser.GameObjects.Arc as Phaser.GameObjects.Arc
import Phaser.GameObjects.Line as Phaser.GameObjects.Line
import Phaser.GameObjects.Rectangle as Phaser.GameObjects.Rectangle
import Phaser.GameObjects.Shape as Phaser.GameObjects.Shape

---------------------------
-- Phaser.GameObjects.Shape
---------------------------
class Shape a where
  toShape :: a -> Phaser.GameObjects.Shape.Shape

instance shapeShape :: Shape Phaser.GameObjects.Shape.Shape where
  toShape = identity

instance shapeArc :: Shape Phaser.GameObjects.Arc.Arc where
  toShape = Phaser.GameObjects.Arc.toShape

instance shapeLine :: Shape Phaser.GameObjects.Line.Line where
  toShape = Phaser.GameObjects.Line.toShape

instance shapeRectangle :: Shape Phaser.GameObjects.Rectangle.Rectangle where
  toShape = Phaser.GameObjects.Rectangle.toShape

setAngle :: forall a. Shape a => a -> { degrees :: Number } -> Effect Unit
setAngle a = Phaser.GameObjects.Shape.setAngle (toShape a)

-- TODO not all Shapes can be filled
setIsFilled :: forall a. Shape a => a -> Boolean -> Effect Unit
setIsFilled a = Phaser.GameObjects.Shape.setIsFilled (toShape a)

-- TODO not all Shape can be stroked
setIsStroked :: forall a. Shape a => a -> Boolean -> Effect Unit
setIsStroked a = Phaser.GameObjects.Shape.setIsStroked (toShape a)

-- TODO not all Shapes can be filled
setFillStyle ::
  forall a.
  Shape a =>
  a ->
  { color :: Int
  , alpha :: Number
  } ->
  Effect Unit
setFillStyle a = Phaser.GameObjects.Shape.setFillStyle (toShape a)

-- TODO not all Shape can be stroked
setStrokeStyle ::
  forall a.
  Shape a =>
  a ->
  { lineWidth :: Number
  , color :: Int
  , alpha :: Number
  } ->
  Effect Unit
setStrokeStyle a = Phaser.GameObjects.Shape.setStrokeStyle (toShape a)
