module Phaser.Math.Vector2
  ( Vector2
  , point
  ) where

foreign import data Vector2 :: Type

foreign import point :: Vector2 -> { x :: Number, y :: Number }
