module Phaser.Input.Pointer
  ( Pointer
  , getInterpolatedPosition
  , point
  ) where

import Data.Function.Uncurried as Data.Function.Uncurried
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried

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

foreign import _point ::
  Data.Function.Uncurried.Fn1
    Pointer
    { x :: Number
    , y :: Number
    }

point :: Pointer -> { x :: Number, y :: Number }
point pointer =
  Data.Function.Uncurried.runFn1
    _point
    pointer
