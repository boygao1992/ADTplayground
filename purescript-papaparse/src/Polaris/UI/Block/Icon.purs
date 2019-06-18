module Polaris.UI.Block.Icon where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)
import Polaris.UI.Block.Icon.Color (Color)
import Polaris.UI.Block.Icon.Color as Color
import Polaris.UI.Block.Icon.Type (IconType)
import Svg.Parser.Halogen (SvgNode)
-- import Polaris.UI.Block.Icon.Type as IconType

_icon = "Polaris-Icon" :: String
_icon_HasBackDrop = "Polaris-Icon--hasBackdrop" :: String
_icon_IsColored = "Polaris-Icon--isColored" :: String

_iconPlaceholder = "Polaris-Icon__Placeholder" :: String
_iconSvg = "Polaris-Icon__Svg" :: String
_iconImg = "Polaris-Icon__Img" :: String

colorToClassName :: Color -> ClassName
colorToClassName = ClassName <<< (_icon <> _) <<< ("--color" <> _) <<< show

data IconSource
  = HTML
  | SVGSource { body :: SvgNode, viewBox :: String }
  | Placeholder
  | BundledIcon IconType
  | UntrustedSVG String

iconClasses
  :: { color :: Maybe Color
    , backdrop :: Boolean
    }
  -> Array ClassName
iconClasses { color: mColor, backdrop }
  = [ ClassName _icon
    ]
    <> case mColor of
       Nothing -> []
       Just color
          -> [ colorToClassName color ]
          <> if color /= Color.White
             then [ClassName _icon_IsColored]
             else []
          <> if backdrop && Color.withBackDrop color
             then [ClassName _icon_HasBackDrop]
             else []

iconSvg
  :: forall p i
  . { color :: Maybe Color
    , backdrop :: Boolean
    }
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
iconSvg opts = blockBuilder HH.div (iconClasses opts)
