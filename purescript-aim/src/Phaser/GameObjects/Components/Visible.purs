module Phaser.GameObjects.Components.Visible
  ( setVisible
  , visible
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject

foreign import _visible ::
  Effect.Uncurried.EffectFn1
    Phaser.GameObjects.GameObject.GameObject
    Boolean

visible :: Phaser.GameObjects.GameObject.GameObject -> Effect Boolean
visible gameObject =
  Effect.Uncurried.runEffectFn1
    _visible
    gameObject

foreign import _setVisible ::
  Effect.Uncurried.EffectFn1
    { gameObject :: Phaser.GameObjects.GameObject.GameObject
    , value :: Boolean
    }
    Unit

setVisible ::
  Phaser.GameObjects.GameObject.GameObject ->
  Boolean ->
  Effect Unit
setVisible gameObject value =
  Effect.Uncurried.runEffectFn1
    _setVisible
    { gameObject
    , value
    }
