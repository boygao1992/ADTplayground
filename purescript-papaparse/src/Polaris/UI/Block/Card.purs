module Polaris.UI.Block.Card where

import Prelude

import DOM.HTML.Indexed
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_card = "Polaris-Card" :: String
_cardSubdued = "Polaris-Card--subdued" :: String

_cardHeader = "Polaris-Card__Header" :: String

_cardSection = "Polaris-Card__Section" :: String
_cardSectionFullWidth = "Polaris-Card__Section--fullWidth" :: String

_cardSubSection = "Polaris-Card__Subsection" :: String
_cardSubSectionSubdued = "Polaris-Card__Section--subdued" :: String

_cardFooter = "Polaris-Card__Footer" :: String
