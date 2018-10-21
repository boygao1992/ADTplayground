module Styles where

import Prelude

import CSS (absolute, black, border, display, em, height, inlineBlock, lightgreen, nil, position, px, relative, select, solid, textWhitespace, top, whitespaceNoWrap, width) as CSS
import CSS.Overflow (hidden, overflow) as CSS
import CSS.Stylesheet (CSS)
import CSSUtils (class_, margin1) as CSS
import ClassNames as CN
import Visibility (visibility, visibilityHidden) as CSS

-- | CSS
container :: CSS
container = do
  CSS.width (CSS.px 600.0)
  CSS.height (CSS.px 200.0)
  CSS.border CSS.solid (CSS.px 3.0) CSS.black
  CSS.position CSS.relative

item :: CSS
item = do
  CSS.display CSS.inlineBlock
  CSS.border CSS.solid (CSS.px 3.0) CSS.lightgreen
  CSS.margin1 (CSS.em 0.2)

ghostItem :: CSS
ghostItem = do
  CSS.display CSS.inlineBlock
  CSS.height CSS.nil
  CSS.overflow CSS.hidden
  CSS.position CSS.absolute
  CSS.top CSS.nil
  CSS.visibility CSS.visibilityHidden
  CSS.textWhitespace CSS.whitespaceNoWrap

root :: CSS
root = do
  CSS.select (CSS.class_ CN.container) container
  CSS.select (CSS.class_ CN.item) item
  CSS.select (CSS.class_ CN.ghostItem) ghostItem

-- | CSS function
inputWidth :: Number -> CSS
inputWidth x = do
  CSS.width (CSS.px x)
