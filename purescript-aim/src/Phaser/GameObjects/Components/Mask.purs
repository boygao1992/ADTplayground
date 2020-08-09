module Phaser.GameObjects.Components.Mask
  ( createGeometryMask
  , setMask
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.Display.Masks.GeometryMask as Phaser.Display.Masks.GeometryMask
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Phaser.GameObjects.Graphics as Phaser.GameObjects.Graphics

foreign import _createGeometryMask ::
  Effect.Uncurried.EffectFn1
    Phaser.GameObjects.Graphics.Graphics
    Phaser.Display.Masks.GeometryMask.GeometryMask

createGeometryMask ::
  Phaser.GameObjects.Graphics.Graphics ->
  Effect Phaser.Display.Masks.GeometryMask.GeometryMask
createGeometryMask graphics =
  Effect.Uncurried.runEffectFn1
    _createGeometryMask
    graphics

foreign import _setMask ::
  Effect.Uncurried.EffectFn1
    { gameObject :: Phaser.GameObjects.GameObject.GameObject
    , mask :: Phaser.Display.Masks.GeometryMask.GeometryMask -- TODO | BitmapMask
    }
    Unit

setMask ::
  Phaser.GameObjects.GameObject.GameObject ->
  Phaser.Display.Masks.GeometryMask.GeometryMask ->
  Effect Unit
setMask gameObject mask =
  Effect.Uncurried.runEffectFn1
    _setMask
    { gameObject
    , mask
    }
