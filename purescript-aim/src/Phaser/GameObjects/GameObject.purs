module Phaser.GameObjects.GameObject
  ( GameObject
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried

foreign import data GameObject :: Type

foreign import _setInteractive_ ::
  Effect.Uncurried.EffectFn1
    GameObject
    Unit

setInteractive_ :: GameObject -> Effect Unit
setInteractive_ gameObject =
  Effect.Uncurried.runEffectFn1
    _setInteractive_
    gameObject

foreign import _setInteractive ::
  -- TODO class Shape
  forall a.
  Effect.Uncurried.EffectFn1
    { gameObject :: GameObject
    , shape :: a
    , callback ::
        Effect.Uncurried.EffectFn1
          { hitArea :: a
          , x :: Number
          , y :: Number
          , gameObject :: GameObject
          }
          Boolean
    , dropZone :: Boolean
    }
    Unit

setInteractive ::
  -- TODO class Shape
  forall a.
  GameObject ->
  { shape :: a
  , callback ::
      { hitArea :: a
      , x :: Number
      , y :: Number
      , gameObject :: GameObject
      } ->
      Effect Boolean
  , dropZone :: Boolean
  } ->
  Effect Unit
setInteractive gameObject { shape, callback, dropZone } =
  Effect.Uncurried.runEffectFn1
    _setInteractive
    { gameObject
    , shape
    , callback: Effect.Uncurried.mkEffectFn1 callback
    , dropZone
    }
