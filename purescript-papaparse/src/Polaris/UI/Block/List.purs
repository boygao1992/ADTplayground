module Polaris.UI.Block.List where

import DOM.HTML.Indexed (HTMLul, HTMLli)
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_list = "Polaris-List" :: String
_list_typeNumber = "Polaris-List--typeNumber" :: String
_item = "Polaris-List__Item" :: String

list
  :: forall p i
  . Array (IProp HTMLul i)
  -> Array (HTML p i)
  -> HTML p i
list = blockBuilder HH.h3 [ ClassName _list ]

list_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
list_ = list []

list_typeNumber
  :: forall p i
  . Array (IProp HTMLul i)
  -> Array (HTML p i)
  -> HTML p i
list_typeNumber = blockBuilder HH.ul [ ClassName _list_typeNumber ]

list_typeNumber_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
list_typeNumber_ = list_typeNumber []

item
  :: forall p i
  . Array (IProp HTMLli i)
  -> Array (HTML p i)
  -> HTML p i
item = blockBuilder HH.li [ ClassName _item ]

item_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
item_ = item []
