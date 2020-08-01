module Phaser.Game
  ( GameConfig
  , Renderer
  , auto
  , newGame
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.Scene as Phaser.Scene

type GameConfig
  = { width :: Number
    , height :: Number
    , type :: Renderer
    , scene :: Array Phaser.Scene.Scene
    , backgroundColor :: Int
    }

foreign import data Renderer :: Type

foreign import auto :: Renderer

foreign import _newGame ::
  Effect.Uncurried.EffectFn1
    GameConfig
    Unit

newGame :: GameConfig -> Effect Unit
newGame config = Effect.Uncurried.runEffectFn1 _newGame config
