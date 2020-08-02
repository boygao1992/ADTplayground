module Index
  ( main
  ) where

import Prelude
import Effect (Effect)
import Example.AimTarget as Example.AimTarget
import Phaser.Game as Phaser.Game

main :: Effect Unit
main = do
  aimTarget <- Example.AimTarget.scene
  Phaser.Game.newGame
    { width: 800.0
    , height: 800.0
    , type: Phaser.Game.auto
    , scene: [ aimTarget ]
    , backgroundColor: 0xffffff
    }
