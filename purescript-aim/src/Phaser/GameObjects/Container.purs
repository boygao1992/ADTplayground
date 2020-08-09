module Phaser.GameObjects.Container
  ( Container
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject

-------------------------------
-- Phaser.GameObjects.Container
-------------------------------
foreign import data Container :: Type

foreign import _add ::
  Effect.Uncurried.EffectFn1
    { container :: Container
    , child :: Phaser.GameObjects.GameObject.GameObject
    }
    Unit

add ::
  Container ->
  Phaser.GameObjects.GameObject.GameObject ->
  Effect Unit
add container child =
  Effect.Uncurried.runEffectFn1
    _add
    { container
    , child
    }

foreign import _setSize ::
  Effect.Uncurried.EffectFn1
    { container :: Container
    , width :: Number
    , height :: Number
    }
    Unit

setSize ::
  Container ->
  { width :: Number
  , height :: Number
  } ->
  Effect Unit
setSize container { width, height } =
  Effect.Uncurried.runEffectFn1
    _setSize
    { container
    , width
    , height
    }
