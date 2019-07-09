module Polaris.UI.Block.Collapsible where

-- import DOM.HTML.Indexed
-- import Halogen.HTML (HTML, IProp, ClassName(..))
-- import Halogen.HTML as HH
-- import Ocelot.Block.Builder (blockBuilder)

_collapsible = "Polaris-Collapsible" :: String
_collapsible_animating = "Polaris-Collapsible--animating" :: String
_collapsible_open = "Polaris-Collapsible--open" :: String
_collapsible_fullyOpen = "Polaris-Collapsible--fullyOpen" :: String

data AnimationState
  = Idle
  | Measuring
  | ClosingStart
  | Closing
  | OpeningStart
  | Opening
