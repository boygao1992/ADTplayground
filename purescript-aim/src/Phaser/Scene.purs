module Phaser.Scene
  ( Scene
  , SceneContext
  , add
  , buildScene
  , graphics
  , input
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObjectFactory as Phaser.GameObjects.GameObjectFactory
import Phaser.GameObjects.Graphics as Phaser.GameObjects.Graphics
import Phaser.Input as Phaser.Input

---------------
-- Phaser.Scene
---------------
foreign import data Scene :: Type

foreign import data SceneContext :: Type

foreign import _buildScene ::
  Effect.Uncurried.EffectFn1
    { preload :: Effect.Uncurried.EffectFn1 SceneContext Unit
    , create :: Effect.Uncurried.EffectFn1 SceneContext Unit
    }
    Scene

buildScene ::
  { preload :: SceneContext -> Effect Unit
  , create :: SceneContext -> Effect Unit
  } ->
  Effect Scene
buildScene { preload, create } =
  Effect.Uncurried.runEffectFn1
    _buildScene
    { preload: Effect.Uncurried.mkEffectFn1 preload
    , create: Effect.Uncurried.mkEffectFn1 create
    }

foreign import _add ::
  Effect.Uncurried.EffectFn1
    SceneContext
    Phaser.GameObjects.GameObjectFactory.GameObjectFactory

add :: SceneContext -> Effect Phaser.GameObjects.GameObjectFactory.GameObjectFactory
add context = Effect.Uncurried.runEffectFn1 _add context

foreign import _graphics ::
  Effect.Uncurried.EffectFn1
    Phaser.GameObjects.GameObjectFactory.GameObjectFactory
    Phaser.GameObjects.Graphics.Graphics

graphics ::
  Phaser.GameObjects.GameObjectFactory.GameObjectFactory ->
  Effect Phaser.GameObjects.Graphics.Graphics
graphics context = Effect.Uncurried.runEffectFn1 _graphics context

foreign import _input ::
  Effect.Uncurried.EffectFn1
    SceneContext
    Phaser.Input.InputPlugin

input :: SceneContext -> Effect Phaser.Input.InputPlugin
input context = Effect.Uncurried.runEffectFn1 _input context
