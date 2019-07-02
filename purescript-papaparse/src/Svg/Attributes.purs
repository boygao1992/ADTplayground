module Svg.Attributes where
-- Like Halogen.HTML.Properties

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (joinWith, toUpper)

import Core as Core

import Halogen.HTML.Core (Prop, AttrName(AttrName))
import Halogen.HTML.Properties (IProp)
import Unsafe.Coerce (unsafeCoerce)

data Color
  = RGB Int Int Int
  | CurrentColor

printColor :: Maybe Color -> String
printColor (Just (RGB _r g b)) = "rgb(" <> (joinWith "," $ map show [_r, g, b]) <> ")"
printColor (Just CurrentColor) = "currentColor"
printColor Nothing = "None"

data Transform
  = Matrix Number Number Number Number Number Number
  | Translate Number Number
  | Scale Number Number
  | Rotate Number Number Number
  | SkewX Number
  | SkewY Number

data TextAnchor = Start | AnchorMiddle | End

printTextAnchor :: TextAnchor -> String
printTextAnchor Start = "start"
printTextAnchor AnchorMiddle = "middle"
printTextAnchor End = "end"

data Baseline
  = Auto
  | UseScript
  | NoChange
  | ResetSize
  | Ideographic
  | Alphabetic
  | Hanging
  | Mathematical
  | Central
  | BaselineMiddle
  | TextAfterEdge
  | TextBeforeEdge
printBaseline :: Baseline -> String
printBaseline Auto = "auto"
printBaseline UseScript = "use-script"
printBaseline NoChange = "no-change"
printBaseline ResetSize = "reset-size"
printBaseline Ideographic = "ideographic"
printBaseline Alphabetic = "alphabetic"
printBaseline Hanging = "hanging"
printBaseline Mathematical = "mathematical"
printBaseline Central = "central"
printBaseline BaselineMiddle = "middle"
printBaseline TextAfterEdge = "text-after-edge"
printBaseline TextBeforeEdge = "text-before-edge"

printTransform :: Transform -> String
printTransform (Matrix a b c _d e f) =
  "matrix(" <> (joinWith "," $ map show [a, b, c, _d, e, f]) <> ")"
printTransform (Translate _x _y) = "translate(" <> (joinWith "," $ map show [_x, _y]) <> ")"
printTransform (Scale _x _y) = "scale(" <> (joinWith "," $ map show [_x, _y]) <> ")"
printTransform (Rotate a _x _y) = "rotate(" <> (joinWith "," $ map show [a, _x, _y]) <> ")"
printTransform (SkewX a) = "skewX(" <> show a <> ")"
printTransform (SkewY a) = "skewY(" <> show a <> ")"

data D = Rel Command | Abs Command
printD :: D -> String
printD (Abs cmd) = (toUpper p.command) <> p.params
  where p = printCommand cmd
printD (Rel cmd) = p.command <> p.params
  where p = printCommand cmd

data Command
  = M Number Number
  | L Number Number
  | C Number Number Number Number Number Number
  | S Number Number Number Number
  | Q Number Number Number Number
  | T Number Number
  | A Number Number Number Boolean Boolean Number Number
  | Z

printCommand :: Command -> {command :: String, params :: String}
printCommand (M _x _y) = {command: "m", params: joinWith "," $ map show [_x, _y]}
printCommand (L _x _y) = {command: "l", params: joinWith "," $ map show [_x, _y]}
printCommand (C _x1 _y1 _x2 _y2 _x _y) =
  {command: "c" , params: joinWith "," $ map show [_x1, _y1, _x2, _y2, _x, _y]}
printCommand (S _x2 _y2 _x _y) =
  {command: "s" , params: joinWith "," $ map show [_x2, _y2, _x, _y]}
printCommand (Q _x1 _y1 _x _y) =
  {command: "q" , params: joinWith "," $ map show [_x1, _y1, _x, _y]}
printCommand (T _x _y) = {command: "t", params: joinWith "," $ map show [_x, _y]}
printCommand (A _rx _ry rot large sweep _x _y) =
  {command: "a", params: joinWith ","
                 $ map show [ _rx, _ry, rot ]
                 <> [ large_flag, sweep_flag ]
                 <> map show [ _x, _y ]}
  where
  large_flag = if large then "0" else "1"
  sweep_flag = if sweep then "0" else "1"
printCommand Z = {command: "z", params: ""}

data Align = Min | Mid | Max
derive instance eqAlign :: Eq Align
derive instance ordAlign :: Ord Align
printAlign :: Align -> String
printAlign Min = "Min"
printAlign Mid = "Mid"
printAlign Max = "Max"

data MeetOrSlice = Meet | Slice
derive instance eqMeetOrSlice :: Eq MeetOrSlice
derive instance ordMeetOrSlice :: Ord MeetOrSlice
printMeetOrSlice :: MeetOrSlice -> String
printMeetOrSlice Meet = "meet"
printMeetOrSlice Slice = "slice"

data FillRule = NonZero | EvenOdd
derive instance eqFillRule :: Eq FillRule
derive instance ordFillRule :: Ord FillRule
printFillRule :: FillRule -> String
printFillRule NonZero = "nonzero"
printFillRule EvenOdd = "evenodd"

newtype ViewBox = ViewBox
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }
derive newtype instance eqViewBox :: Eq ViewBox
derive newtype instance showViewBox :: Show ViewBox
printViewBox :: ViewBox -> String
printViewBox (ViewBox { x: _x, y: _y, w: _w, h: _h })
  = joinWith " "
    <<< map show
    $ [_x, _y, _w, _h]

type Align2D =
  { x :: Align
  , y :: Align
  }
printAlign2D :: Maybe Align2D -> String
printAlign2D (Just {x: _x, y: _y})
  = joinWith "" ["x", printAlign _x, "Y", printAlign _y]
printAlign2D Nothing = "none"

newtype PreserveAspectRatio = PreserveAspectRatio
  { align :: Maybe Align2D
  , meetOrSlice :: MeetOrSlice
  }
printPreserveAspectRatio :: PreserveAspectRatio -> String
printPreserveAspectRatio (PreserveAspectRatio { align, meetOrSlice })
  = joinWith " " [printAlign2D align, printMeetOrSlice meetOrSlice]

attr :: forall r i. AttrName -> String -> IProp r i
attr = coe Core.attr
  where
    coe :: (AttrName -> String -> Prop i) -> AttrName -> String -> IProp r i
    coe = unsafeCoerce

cx :: forall r i. Number -> IProp (cx :: Number | r) i
cx = attr (AttrName "cx") <<< show

cy :: forall r i. Number -> IProp (cy :: Number | r) i
cy = attr (AttrName "cy") <<< show

r :: forall s i. Number -> IProp (r :: Number | s) i
r = attr (AttrName "r") <<< show

viewBox :: forall r i. Number -> Number -> Number -> Number -> IProp (viewBox :: ViewBox | r) i
viewBox _x _y _w _h = attr (AttrName "viewBox") (printViewBox $ ViewBox {x: _x, y: _y, w: _w, h: _h})

preservePreserveAspectRatio :: forall r i. Maybe Align2D -> MeetOrSlice -> IProp (preservePreserveAspectRatio :: PreserveAspectRatio | r) i
preservePreserveAspectRatio align meetOrSlice =
  attr (AttrName "preservePreserveAspectRatio")
  (printPreserveAspectRatio $ PreserveAspectRatio {align, meetOrSlice})

rx :: forall r i. Number -> IProp (rx :: Number | r) i
rx = attr (AttrName "rx") <<< show

ry :: forall r i. Number -> IProp (ry :: Number | r) i
ry = attr (AttrName "ry") <<< show

width :: forall r i. Number -> IProp (width :: Number | r) i
width = attr (AttrName "width") <<< show

height :: forall r i. Number -> IProp (height :: Number | r) i
height = attr (AttrName "height") <<< show

x :: forall r i. Number -> IProp (x :: Number | r) i
x = attr (AttrName "x") <<< show

y :: forall r i. Number -> IProp (y :: Number | r) i
y = attr (AttrName "y") <<< show

x1 :: forall r i. Number -> IProp (x1 :: Number | r) i
x1 = attr (AttrName "x1") <<< show

y1 :: forall r i. Number -> IProp (y1 :: Number | r) i
y1 = attr (AttrName "y1") <<< show

x2 :: forall r i. Number -> IProp (x2 :: Number | r) i
x2 = attr (AttrName "x2") <<< show

y2 :: forall r i. Number -> IProp (y2 :: Number | r) i
y2 = attr (AttrName "y2") <<< show

stroke :: forall r i. Maybe Color -> IProp (stroke :: Color | r) i
stroke = attr (AttrName "stroke") <<< printColor

fill :: forall r i. Maybe Color -> IProp (fill :: Color | r) i
fill = attr (AttrName "fill") <<< printColor

fillRule :: forall r i. FillRule -> IProp (fillRule :: FillRule | r) i
fillRule = attr (AttrName "fill-rule") <<< printFillRule

transform :: forall r i . Array Transform -> IProp (transform :: Transform | r) i
transform = attr (AttrName "transform") <<< joinWith " " <<< map printTransform

d :: forall r i . Array D -> IProp (d :: D | r) i
d = attr (AttrName "d") <<< joinWith " " <<< map printD

unsafeD :: forall r i. String -> IProp (d :: D | r) i
unsafeD = attr (AttrName "d")

textAnchor :: forall r i . TextAnchor -> IProp (textAnchor :: TextAnchor | r) i
textAnchor = attr (AttrName "text-anchor") <<< printTextAnchor

dominantBaseline :: forall r i . Baseline -> IProp (dominantBaseline :: Baseline | r) i
dominantBaseline = attr (AttrName "dominant-baseline") <<< printBaseline

class_ :: forall r i . String -> IProp (class :: String | r) i
class_ = attr (AttrName "class")

id :: forall r i . String -> IProp (id :: String | r) i
id = attr (AttrName "id")
