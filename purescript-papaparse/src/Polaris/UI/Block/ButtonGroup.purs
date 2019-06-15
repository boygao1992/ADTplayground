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

groupClasses
  :: { segmented :: Boolean
    , fullWidth :: Boolean
    , connectedTop :: Boolean
    }
  -> Array ClassName
groupClasses { segmented, fullWidth, connectedTop }
  = ClassName <$>
    [ _group
    ]
    <> if segmented then [_groupSegmented] else []
    <> if fullWidth then [_groupFullWidth] else []
    <> if connectedTop then [_groupConnectedTop] else []

itemClasses
  :: { plain :: Boolean
    , focused :: Boolean
    }
  -> Array ClassName
itemClasses { plain, focused }
  = ClassName <$>
  [ _item
  ]
  <> if plain then [_itemPlain] else []
  <> if focused then [_itemFocused] else []

buttonGroup
  :: forall p i
  . { segmented :: Boolean
    , fullWidth :: Boolean
    , connectedTop :: Boolean
    }
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
buttonGroup opts = blockBuilder HH.div (groupClasses opts)

buttonGroupItem
  :: forall p i
  . { plain :: Boolean
    , focused :: Boolean
    }
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
buttonGroupItem opts = blockBuilder HH.div (itemClasses opts)
