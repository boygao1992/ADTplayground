module CSSUtils where

import Prelude
import CSS.Selector (Predicate(Pseudo, Class, Id), Refinement(Refinement), Selector)
import CSS.Selector (star, with, deep) as CSS
import CSS.Size (Size)
import CSS (margin, padding) as CSS
import CSS.Stylesheet (CSS, key)
import CSS.String (fromString)

-- | Util
infix 6 CSS.with as & -- AB, bug fix for 3.4.0
infix 6 CSS.deep as |* -- A B

byId :: String -> Refinement
byId = Refinement <<< pure <<< Id

byClass :: String -> Refinement
byClass = Refinement <<< pure <<< Class

pseudo :: String -> Refinement
pseudo = Refinement <<< pure <<< Pseudo

id_ :: String -> Selector
id_ s = CSS.star & byId s

class_ :: String -> Selector
class_ s = CSS.star & byClass s

margin1 :: forall a. Size a -> CSS
margin1 s = CSS.margin s s s s

margin2 :: forall a. Size a -> Size a -> CSS
margin2 s1 s2 = CSS.margin s1 s2 s1 s2

padding1 :: forall a. Size a -> CSS
padding1 s = CSS.padding s s s s

padding2 :: forall a. Size a -> Size a -> CSS
padding2 s1 s2 = CSS.padding s1 s2 s1 s2

borderWidth :: forall a. Size a -> CSS
borderWidth = key $ fromString "border-width"
