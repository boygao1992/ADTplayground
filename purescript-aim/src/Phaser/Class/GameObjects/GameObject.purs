module Phaser.Class.GameObjects.GameObject
  ( setInteractive_
  , class GameObject
  , toGameObject
  ) where

import Prelude
import Effect (Effect)
import Phaser.GameObjects.Arc as Phaser.GameObjects.Arc
import Phaser.GameObjects.DOMElement as Phaser.GameObjects.DOMElement
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Phaser.GameObjects.Graphics as Phaser.GameObjects.Graphics
import Phaser.GameObjects.Line as Phaser.GameObjects.Line
import Phaser.GameObjects.Rectangle as Phaser.GameObjects.Rectangle
import Phaser.GameObjects.Shape as Phaser.GameObjects.Shape

--------------------------------
-- Phaser.GameObjects.GameObject
--------------------------------
class GameObject a where
  toGameObject :: a -> Phaser.GameObjects.GameObject.GameObject

instance gameObjectGameObject :: GameObject Phaser.GameObjects.GameObject.GameObject where
  toGameObject = identity

instance gameObjectDOMElement :: GameObject Phaser.GameObjects.DOMElement.DOMElement where
  toGameObject = Phaser.GameObjects.DOMElement.toGameObject

instance gameObjectGraphics :: GameObject Phaser.GameObjects.Graphics.Graphics where
  toGameObject = Phaser.GameObjects.Graphics.toGameObject

instance gameObjectShape :: GameObject Phaser.GameObjects.Shape.Shape where
  toGameObject = Phaser.GameObjects.Shape.toGameObject

instance gameObjectArc :: GameObject Phaser.GameObjects.Arc.Arc where
  toGameObject =
    toGameObject
      <<< Phaser.GameObjects.Arc.toShape

instance gameObjectLine :: GameObject Phaser.GameObjects.Line.Line where
  toGameObject =
    toGameObject
      <<< Phaser.GameObjects.Line.toShape

instance gameObjectRectangle :: GameObject Phaser.GameObjects.Rectangle.Rectangle where
  toGameObject =
    toGameObject
      <<< Phaser.GameObjects.Rectangle.toShape

setInteractive_ :: forall a. GameObject a => a -> Effect Unit
setInteractive_ a = Phaser.GameObjects.GameObject.setInteractive_ (toGameObject a)
