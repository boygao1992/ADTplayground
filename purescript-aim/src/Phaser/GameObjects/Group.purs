module Phaser.GameObjects.Group
  ( Group
  , add
  , addMultiple
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject

---------------------------
-- Phaser.GameObjects.Group
---------------------------
foreign import data Group :: Type

foreign import _add ::
  Effect.Uncurried.EffectFn1
    { group :: Group
    , child :: Phaser.GameObjects.GameObject.GameObject
    , addToScene :: Boolean
    }
    Unit

add ::
  Group ->
  Phaser.GameObjects.GameObject.GameObject ->
  Effect Unit
add group child =
  Effect.Uncurried.runEffectFn1
    _add
    { group
    , child
    , addToScene: false
    }

foreign import _addMultiple ::
  Effect.Uncurried.EffectFn1
    { group :: Group
    , children :: Array Phaser.GameObjects.GameObject.GameObject
    , addToScene :: Boolean
    }
    Unit

addMultiple ::
  Group ->
  Array Phaser.GameObjects.GameObject.GameObject ->
  Effect Unit
addMultiple group children =
  Effect.Uncurried.runEffectFn1
    _addMultiple
    { group
    , children
    , addToScene: false
    }
