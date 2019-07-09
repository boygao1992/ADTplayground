module Polaris.UI.Block.Heading where

import DOM.HTML.Indexed (HTMLh2, HTMLp)
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

heading_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
heading_ = heading []

headingP
  :: forall p i
  . Array (IProp HTMLp i)
  -> Array (HTML p i)
  -> HTML p i
headingP = blockBuilder HH.p [ ClassName _heading ]

headingP_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
headingP_ = headingP []
