module Phaser.GameObjects.RenderTexture
  ( RenderTexture
  , clear
  , draw
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject

-----------------------------------
-- Phaser.GameObjects.RenderTexture
-----------------------------------
foreign import data RenderTexture :: Type

foreign import _clear ::
  Effect.Uncurried.EffectFn1
    RenderTexture
    Unit

clear :: RenderTexture -> Effect Unit
clear renderTexture =
  Effect.Uncurried.runEffectFn1
    _clear
    renderTexture

foreign import _draw ::
  Effect.Uncurried.EffectFn1
    { renderTexture :: RenderTexture
    , entries :: Phaser.GameObjects.GameObject.GameObject
    , x :: Number
    , y :: Number
    , alpha :: Number
    }
    Unit

{- Draws the given object, or an array of objects, to this Render Texture.
  1. Any renderable Game Object, such as a Sprite, Text, Graphics or TileSprite.
  2. Dynamic and Static Tilemap Layers.
  3. A Group. The contents of which will be iterated and drawn in turn.
  4. A Container. The contents of which will be iterated fully, and drawn in turn.
  5. A Scene's Display List. Pass in Scene.children to draw the whole list.
  6. Another Render Texture. Note: You cannot draw a Render Texture to itself.
  7. A Texture Frame instance.
  8. A string. This is used to look-up a texture from the Texture Manager.
-}
draw ::
  RenderTexture ->
  Phaser.GameObjects.GameObject.GameObject ->
  { x :: Number
  , y :: Number
  , alpha :: Number
  } ->
  Effect Unit
draw renderTexture entries { x, y, alpha } =
  Effect.Uncurried.runEffectFn1
    _draw
    { renderTexture
    , entries
    , x
    , y
    , alpha
    }
