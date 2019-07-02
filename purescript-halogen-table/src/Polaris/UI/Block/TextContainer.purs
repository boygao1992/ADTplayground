module Polaris.UI.Block.TextContainer where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

_textcontainer = "Polaris-TextContainer" :: String
_textcontainer_spacingTight = "Polaris-TextContainer--spacingTight" :: String
_textcontainer_spacingLoose = "Polaris-TextContainer--spacingLoose" :: String

defaultOptions :: Spacing
defaultOptions = Normal

data Spacing
  = Normal
  | Tight
  | Loose
derive instance genericSpacing :: Generic Spacing _
derive instance eqSpacing :: Eq Spacing
derive instance ordSpacing :: Ord Spacing
instance showSpacing :: Show Spacing where
  show = genericShow

textContainer
  :: forall p i
  . Spacing
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
textContainer spacing = blockBuilder HH.div
  [ ClassName case spacing of
      Normal -> _textcontainer
      Tight -> _textcontainer_spacingTight
      Loose -> _textcontainer_spacingLoose
  ]

textContainer_
  :: forall p i
  . Spacing
  -> Array (HTML p i)
  -> HTML p i
textContainer_ spacing = textContainer spacing []
