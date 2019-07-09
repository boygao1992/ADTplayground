module Polaris.UI.Block.Subheading where

import DOM.HTML.Indexed (HTMLh3)
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_subheading = "Polaris-Subheading" :: String

subheading
  :: forall p i
  . Array (IProp HTMLh3 i)
  -> Array (HTML p i)
  -> HTML p i
subheading = blockBuilder HH.h3 [ ClassName _subheading ]

subheading_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
subheading_ = subheading []
