module Polaris.UI.Block.Card where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML (ClassName(..), HTML, IProp)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_card = "Polaris-Card" :: String
_card_subdued = "Polaris-Card--subdued" :: String

_cardHeader = "Polaris-Card__Header" :: String

_cardSection = "Polaris-Card__Section" :: String
_cardSection_fullWidth = "Polaris-Card__Section--fullWidth" :: String
_cardSection_subdued = "Polaris-Card__Section--subdued" :: String

_cardSubSection = "Polaris-Card__Subsection" :: String

_cardSectionHeader = "Polaris-Card__SectionHeader" :: String

_cardFooter = "Polaris-Card__Footer" :: String

card
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
card = blockBuilder HH.div [ ClassName _card ]

card_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
card_ = card []

card_subdued
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
card_subdued = blockBuilder HH.div [ ClassName _card_subdued ]

card_subdued_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
card_subdued_ = card_subdued []

header
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
header = blockBuilder HH.div [ ClassName _cardHeader ]

header_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
header_ = header []

section
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
section = blockBuilder HH.div [ ClassName _cardSection ]

section_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
section_ = section []

subSection
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
subSection = blockBuilder HH.div [ ClassName _cardSubSection ]

subSection_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
subSection_ = subSection []

section_subdued
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
section_subdued = blockBuilder HH.div $ ClassName <$>
  [ _cardSection
  , _cardSection_subdued
  ]

section_subdued_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
section_subdued_ = section_subdued []

sectionHeader
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
sectionHeader = blockBuilder HH.div [ ClassName _cardSectionHeader ]

sectionHeader_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
sectionHeader_ = sectionHeader []

footer
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
footer = blockBuilder HH.div [ ClassName _cardFooter ]

footer_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
footer_ = footer []
