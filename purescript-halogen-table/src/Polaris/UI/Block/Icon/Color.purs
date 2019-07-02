module Polaris.UI.Block.Icon.Color where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Color
  = Black
  | BlueLighter
  | BlueLight
  | Blue
  | BlueDark
  | BlueDarker
  | GreenLighter
  | Green
  | GreenDark
  | InkLightest
  | InkLighter
  | InkLight
  | Ink
  | IndigoLighter
  | IndigoLight
  | Indigo
  | IndigoDark
  | IndigoDarker
  | Orange
  | Purple
  | RedLighter
  | Red
  | RedDark
  | SkyLighter
  | SkyLight
  | Sky
  | SkyDark
  | TealLighter
  | TealLight
  | Teal
  | TealDark
  | TealDarker
  | White
  | YellowLighter
  | Yellow
  | YellowDark
derive instance genericColor :: Generic Color _
derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color
instance showColor :: Show Color where
  show x = genericShow x

colorsWithBackDrops :: Array Color
colorsWithBackDrops =
  [ GreenDark
  , Ink
  , InkLighter
  , RedDark
  , Teal
  , TealDark
  , YellowDark
  ]

withBackDrop :: Color -> Boolean
withBackDrop = Array.elem <@> colorsWithBackDrops
