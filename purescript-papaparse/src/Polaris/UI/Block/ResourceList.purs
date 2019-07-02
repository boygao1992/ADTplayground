module Polaris.UI.Block.ResourceList where

-- import DOM.HTML.Indexed
-- import Halogen.HTML (HTML, IProp, ClassName(..))
-- import Halogen.HTML as HH
-- import Ocelot.Block.Builder (blockBuilder)

_resourcelist = "Polaris-ResourceList" :: String
_resourcelist_disableTextSelection = "Polaris-ResourceList--disableTextSelection" :: String

_resourcelistResourceListWrapper = "Polaris-ResourceList__ResourceListWrapper" :: String

_resourcelistItem = "Polaris-ResourceList-Item" :: String
_resourcelistItem_focused = "Polaris-ResourceList-Item--focused" :: String
_resourcelistItem_persistActions = "Polaris-ResourceList-Item--persistActions" :: String
_resourcelistItemWrapper = "Polaris-ResourceList__ItemWrapper" :: String
_resourcelistItemWrapper_isLoading = "Polaris-ResourceList__ItemWrapper--isLoading" :: String

_resourcelistHeaderWrapper = "Polaris-ResourceList__HeaderWrapper" :: String
_resourcelistHeaderWrapper_isSticky = "Polaris-ResourceList__HeaderWrapper--isSticky" :: String
_resourcelistHeaderWrapper_disabled = "Polaris-ResourceList__HeaderWrapper--disabled" :: String
_resourcelistHeaderWrapper_overlay = "Polaris-ResourceList__HeaderWrapper--overlay" :: String
_resourcelistHeaderTitleWrapper = "Polaris-ResourceList__HeaderTitleWrapper" :: String
_resourcelistHeaderContentWrapper = "Polaris-ResourceList__HeaderContentWrapper" :: String

_resourcelistBulkActionsWrapper = "Polaris-ResourceList__BulkActionsWrapper" :: String

_resourcelistCheckableButtonWrapper = "Polaris-ResourceList__CheckableButtonWrapper" :: String

_resourcelistSelectButtonWrapper = "Polaris-ResourceList__SelectButtonWrapper" :: String

_resourcelistEmptySearchResultWrapper = "Polaris-ResourceList__EmptySearchResultWrapper" :: String


-- subheading
--   :: forall p i
--   . Array (IProp HTMLh3 i)
--   -> Array (HTML p i)
--   -> HTML p i
-- subheading = blockBuilder HH.h3 [ ClassName _subheading ]

-- subheading_
--   :: forall p i
--   . Array (HTML p i)
--   -> HTML p i
-- subheading_ = subheading []
