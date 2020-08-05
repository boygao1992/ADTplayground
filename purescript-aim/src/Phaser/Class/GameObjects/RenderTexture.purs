module Phaser.Class.GameObjects.RenderTexture
  ( draw
  ) where

import Prelude
import Effect (Effect)
import Phaser.Class.GameObjects.GameObject as Phaser.Class.GameObjects.GameObject
import Phaser.GameObjects.RenderTexture as Phaser.GameObjects.RenderTexture

draw ::
  forall a.
  Phaser.Class.GameObjects.GameObject.GameObject a =>
  Phaser.GameObjects.RenderTexture.RenderTexture ->
  a ->
  { x :: Number
  , y :: Number
  , alpha :: Number
  } ->
  Effect Unit
draw renderTexture entries =
  Phaser.GameObjects.RenderTexture.draw
    renderTexture
    (Phaser.Class.GameObjects.GameObject.toGameObject entries)
