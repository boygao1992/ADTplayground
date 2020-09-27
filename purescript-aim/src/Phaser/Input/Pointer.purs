module Phaser.Input.Pointer
  ( Pointer
  , getInterpolatedPosition
  , position
  , prevPosition
  ) where

import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.Math.Vector2 as Phaser.Math

-----------------------
-- Phaser.Input.Pointer
-----------------------
foreign import data Pointer :: Type

foreign import _getInterpolatedPosition ::
  Effect.Uncurried.EffectFn1
    { pointer :: Pointer
    , steps :: Int
    }
    ( Array
        { x :: Number
        , y :: Number
        }
    )

getInterpolatedPosition ::
  Pointer ->
  Int ->
  Effect
    ( Array
        { x :: Number
        , y :: Number
        }
    )
getInterpolatedPosition pointer steps =
  Effect.Uncurried.runEffectFn1
    _getInterpolatedPosition
    { pointer
    , steps
    }

foreign import _position ::
  Effect.Uncurried.EffectFn1
    Pointer
    Phaser.Math.Vector2

position :: Pointer -> Effect Phaser.Math.Vector2
position pointer =
  Effect.Uncurried.runEffectFn1
    _position
    pointer

foreign import _prevPosition ::
  Effect.Uncurried.EffectFn1
    Pointer
    Phaser.Math.Vector2

prevPosition :: Pointer -> Effect Phaser.Math.Vector2
prevPosition pointer =
  Effect.Uncurried.runEffectFn1
    _prevPosition
    pointer
