module Polaris.UI.Block.Button where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_button = "Polaris-Button" :: String
_buttonPrimary = "Polaris-Button--primary" :: String
_buttonOutline = "Polaris-Button--outline" :: String
_buttonDestructive = "Polaris-Button--destructive" :: String
_buttonDisabled = "Polaris-Button--disabled" :: String
_buttonLoading = "Polaris-Button--loading" :: String
_buttonPlain = "Polaris-Button--plain" :: String
_buttonMonochrome = "Polaris-Button--monochrome" :: String
_buttonFullWidth = "Polaris-Button--fullWidth" :: String
_buttonIconOnly = "Polaris-Button--iconOnly" :: String

data Size
  = SizeSlim
  | SizeMedium
  | SizeLarge
derive instance eqSize :: Eq Size
derive instance ordSize :: Ord Size
-- NOTE default_size = Medium
_buttonSizeSlim = "Polaris-Button--sizeSlim" :: String
_buttonSizeLarge = "Polaris-Button--sizeLarge" :: String

data TextAlign
  = TextAlignLeft
  | TextAlignCenter
  | TextAlignRight
derive instance eqTextAlign :: Eq TextAlign
derive instance ordTextAlign :: Ord TextAlign

_buttonTextAlignLeft = "Polaris-Button--textAlignLeft" :: String
_buttonTextAlignCenter = "Polaris-Button--textAlignCenter" :: String
_buttonTextAlignRight = "Polaris-Button--textAlignRight" :: String

type ButtonStyleOptions =
  { primary :: Boolean
  , outline :: Boolean
  , destructive :: Boolean
  , isDisabled :: Boolean
  , loading :: Boolean
  , plain :: Boolean
  , monochrome :: Boolean
  , size :: Size
  , textAlign :: TextAlign
  , fullWidth :: Boolean
  , iconOnly :: Boolean
  }

defaultButtonStyleOptions :: ButtonStyleOptions
defaultButtonStyleOptions =
  { primary: false
  , outline: false
  , destructive: false
  , isDisabled: false
  , loading: false
  , plain: false
  , monochrome: false
  , size: SizeMedium
  , textAlign: TextAlignCenter
  , fullWidth: false
  , iconOnly: false
  }

buttonClasses
  :: { primary :: Boolean
    , outline :: Boolean
    , destructive :: Boolean
    , isDisabled :: Boolean
    , loading :: Boolean
    , plain :: Boolean
    , monochrome :: Boolean
    , size :: Size
    , textAlign :: TextAlign
    , fullWidth :: Boolean
    , iconOnly :: Boolean
    }
  -> Array ClassName
buttonClasses { primary, outline, destructive, isDisabled, loading, plain, monochrome, size, textAlign, fullWidth, iconOnly }
  = ClassName <$>
    [ _button
    ]
    <> if primary then [_buttonPrimary] else []
    <> if outline then [_buttonOutline] else []
    <> if destructive then [_buttonDestructive] else []
    <> if isDisabled then [_buttonDisabled] else []
    <> if loading then [_buttonLoading] else []
    <> if plain then [_buttonPlain] else []
    <> if monochrome then [_buttonMonochrome] else []
    <> case size of
        SizeSlim -> [_buttonSizeSlim]
        SizeLarge -> [_buttonSizeLarge]
        SizeMedium -> []
    <> case textAlign of
        TextAlignLeft -> [_buttonTextAlignLeft]
        TextAlignCenter -> [_buttonTextAlignCenter]
        TextAlignRight -> [_buttonTextAlignRight]
    <> if fullWidth then [_buttonFullWidth] else []
    <> if iconOnly then [_buttonIconOnly] else []

button
  :: forall p i
  . { primary :: Boolean
    , outline :: Boolean
    , destructive :: Boolean
    , isDisabled :: Boolean
    , loading :: Boolean
    , plain :: Boolean
    , monochrome :: Boolean
    , size :: Size
    , textAlign :: TextAlign
    , fullWidth :: Boolean
    , iconOnly :: Boolean
    }
  -> Array (IProp HTMLbutton i)
  -> Array (HTML p i)
  -> HTML p i
button opts = blockBuilder HH.button (buttonClasses opts)
