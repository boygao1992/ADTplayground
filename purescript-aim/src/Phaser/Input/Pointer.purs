module Phaser.Input.Pointer
  ( Pointer
  , point
  ) where

import Data.Function.Uncurried as Data.Function.Uncurried

-----------------------
-- Phaser.Input.Pointer
-----------------------
foreign import data Pointer :: Type

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
