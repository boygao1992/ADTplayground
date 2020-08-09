module Phaser.GameObjects.Components.Transform
  ( setPosition
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject

foreign import _setPosition ::
  Effect.Uncurried.EffectFn1
    { gameObject :: Phaser.GameObjects.GameObject.GameObject
    , x :: Number
    , y :: Number
    }
    Unit

setPosition ::
  Phaser.GameObjects.GameObject.GameObject ->
  { x :: Number
  , y :: Number
  } ->
  Effect Unit
setPosition gameObject { x, y } =
  Effect.Uncurried.runEffectFn1
    _setPosition
    { gameObject
    , x
    , y
    }
