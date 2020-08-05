module Phaser.GameObjects.RenderTexture
  ( RenderTexture
  , draw
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject

foreign import data RenderTexture :: Type

foreign import _draw ::
  Effect.Uncurried.EffectFn1
    { renderTexture :: RenderTexture
    , entries :: Phaser.GameObjects.GameObject.GameObject
    , x :: Number
    , y :: Number
    , alpha :: Number
    }
    Unit

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
