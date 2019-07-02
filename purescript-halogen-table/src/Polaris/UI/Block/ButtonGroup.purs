module Polaris.UI.Block.ButtonGroup where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_group = "Polaris-ButtonGroup" :: String
_groupSegmented = "Polaris-ButtonGroup--segmented" :: String
_groupFullWidth = "Polaris-ButtonGroup--fullWidth" :: String
_groupConnectedTop = "Polaris-ButtonGroup--connectedTop" :: String

_item = "Polaris-ButtonGroup__Item" :: String
_itemPlain = "Polaris-ButtonGroup__Item--plain" :: String
_itemFocused = "Polaris-ButtonGroup__Item--focused" :: String

type GroupOptions =
  { segmented :: Boolean
  , fullWidth :: Boolean
  , connectedTop :: Boolean
  }

groupDefaultOptions :: GroupOptions
groupDefaultOptions =
  { segmented: false
  , fullWidth: false
  , connectedTop: false
  }

type ItemOptions =
  { plain :: Boolean
  , focused :: Boolean
  }

itemDefaultOptions :: ItemOptions
itemDefaultOptions =
  { plain: false
  , focused: false
  }

groupClasses :: GroupOptions -> Array ClassName
groupClasses { segmented, fullWidth, connectedTop }
  = ClassName <$>
    [ _group
    ]
    <> if segmented then [_groupSegmented] else []
    <> if fullWidth then [_groupFullWidth] else []
    <> if connectedTop then [_groupConnectedTop] else []

itemClasses :: ItemOptions -> Array ClassName
itemClasses { plain, focused }
  = ClassName <$>
  [ _item
  ]
  <> if plain then [_itemPlain] else []
  <> if focused then [_itemFocused] else []

group
  :: forall p i
  . GroupOptions
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
group opts = blockBuilder HH.div (groupClasses opts)

group_
  :: forall p i
  . GroupOptions
  -> Array (HTML p i)
  -> HTML p i
group_ opts = group opts []

item
  :: forall p i
  . ItemOptions
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
item opts = blockBuilder HH.div (itemClasses opts)

item_
  :: forall p i
  . ItemOptions
  -> Array (HTML p i)
  -> HTML p i
item_ opts = item opts []
