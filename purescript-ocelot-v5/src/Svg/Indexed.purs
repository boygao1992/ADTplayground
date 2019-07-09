module Svg.Indexed where

import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Svg.Attributes

-- Attributes based on Mozilla MDN categories

type CoreAttributes r = (id :: String | r)

type StyleAttributes r = ("class" :: String | r)

-- Subset of events that work on Firefox 60/Chromium 66
type GlobalEventAttributes r =
  ( onClick :: MouseEvent
  , onDoubleClick :: MouseEvent
  , onContextMenu :: MouseEvent
  , onKeyDown :: KeyboardEvent
  , onKeyPress :: KeyboardEvent
  , onKeyUp :: KeyboardEvent
  , onMouseDown :: MouseEvent
  , onMouseEnter :: MouseEvent
  , onMouseLeave :: MouseEvent
  , onMouseMove :: MouseEvent
  , onMouseOut :: MouseEvent
  , onMouseOver :: MouseEvent
  , onMouseUp :: MouseEvent
  , onWheel :: WheelEvent
  | r)

-- These can also be done with CSS
type PresentationAttributes r =
  ( stroke :: Color
  , fill :: Color
  , fillRule :: FillRule
  | r
  )

type GlobalAttributes r = (PresentationAttributes (GlobalEventAttributes (StyleAttributes (CoreAttributes r))))

type SVGsvg = GlobalAttributes
  ( width :: Number
  , height :: Number
  , viewBox :: ViewBox
  , preserveAspectRatio :: PreserveAspectRatio
  )

type SVGcircle = GlobalAttributes
  ( cx :: Number
  , cy :: Number
  , r :: Number
  , transform :: Transform
  )

type SVGellipse = GlobalAttributes
  ( cx :: Number
  , cy :: Number
  , rx :: Number
  , ry :: Number
  , transform :: Transform
  )

type SVGrect = GlobalAttributes
  ( x :: Number
  , y :: Number
  , rx :: Number
  , ry :: Number
  , width :: Number
  , height :: Number
  , transform :: Transform
  )

type SVGg = GlobalAttributes
  ( transform :: Transform )

type SVGpath = GlobalAttributes
  ( d :: D
  , transform :: Transform
  )

type SVGline = GlobalAttributes
  ( x1 :: Number
  , y1 :: Number
  , x2 :: Number
  , y2 :: Number
  , transform :: Transform
  )

type SVGtext = GlobalAttributes
  ( x :: Number
  , y :: Number
  , textAnchor :: TextAnchor
  , dominantBaseline :: Baseline
  , transform :: Transform
  )

type SVGforeignObject = GlobalAttributes
  ( x :: Number
  , y :: Number
  , height :: Number
  , width :: Number
)
