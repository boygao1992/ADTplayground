module Polaris.UI.Block.Tabs where

import Prelude

import DOM.HTML.Indexed (HTMLbutton, HTMLdiv, HTMLli, HTMLspan, HTMLul)
import Data.Monoid (guard)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_tabs = "Polaris-Tabs" :: String
_tabs_fitted = "Polaris-Tabs--fitted" :: String
_tabs_fillSpace = "Polaris-Tabs--fillSpace" :: String

_tabsContainer = "Polaris-Tabs__TabContainer" :: String

_tabsTab = "Polaris-Tabs__Tab" :: String
_tabsTab_selected = "Polaris-Tabs__Tab--selected" :: String

_tabsTitle = "Polaris-Tabs__Title" :: String

_tabsPanel = "Polaris-Tabs__Panel" :: String

_tabsList = "Polaris-Tabs__List" :: String

_tabsItem = "Polaris-Tabs__Item" :: String

_tabsDisclosureTab = "Polaris-Tabs__DisclosureTab" :: String
_tabsDisclosureTab_visible = "Polaris-Tabs__DisclosureTab--visible" :: String

_tabsDiscloureActivator = "Polaris-Tabs__DisclosureActivator" :: String

_tabsTabMeasurer = "Polaris-Tabs__TabMeasurer" :: String

list
  :: forall p i
  . Array (HH.IProp HTMLul i)
  -> Array (HTML p i)
  -> HTML p i
list = blockBuilder HH.ul $ HH.ClassName <$> [ _tabs ]

list_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
list_ = list []

container
  :: forall p i
  . Array (HH.IProp HTMLli i)
  -> Array (HTML p i)
  -> HTML p i
container = blockBuilder HH.li (HH.ClassName <$> [ _tabsContainer ])

container_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
container_ = container []

item
  :: forall p i
  . Boolean
  -> Array (HH.IProp HTMLbutton i)
  -> Array (HTML p i)
  -> HTML p i
item selected = blockBuilder HH.button $ HH.ClassName <$>
  [ _tabsTab
  , _tabsTab_selected # guard selected
  ]

item_
  :: forall p i
  . Boolean
  -> Array (HTML p i)
  -> HTML p i
item_ selected = item selected []

title
  :: forall p i
  . Array (HH.IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
title = blockBuilder HH.span $ HH.ClassName <$>
  [ _tabsTitle
  ]

title_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
title_ = title []

panel
  :: forall p i
  . Array (HH.IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
panel = blockBuilder HH.div $ HH.ClassName <$> [ _tabsPanel ]

panel_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
panel_ = panel []
