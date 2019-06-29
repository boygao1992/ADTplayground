module Polaris.UI.Block.Heading where

import DOM.HTML.Indexed (HTMLh2)
import Halogen.HTML (ClassName(..), HTML, IProp)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_heading = "Polaris-Heading" :: String

heading
  :: forall p i
  . Array (IProp HTMLh2 i)
  -> Array (HTML p i)
  -> HTML p i
heading = blockBuilder HH.h2 [ ClassName _heading ]
