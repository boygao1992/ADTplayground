module Polaris.UI.Block.Button where

import Prelude

import DOM.HTML.Indexed (HTMLbutton, HTMLspan)
import Data.Monoid (guard)
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Ocelot.Block.Builder (blockBuilder)

_button = "Polaris-Button" :: String

_buttonContent = "Polaris-Button__Content" :: String

_buttonText = "Polaris-Button__Text" :: String

_buttonIcon = "Polaris-Button__Icon" :: String

_buttonSpinner = "Polaris-Button__Spinner" :: String

{- Combinations

basic (bg: white, border: gray, font: black)
primary (bg: purple, border: dark_purple, font: white)
destructive (bg: red, border: dark_red, font: white)

plain (bg: transparent, border: none, font: blue)
destructive, plain (bg: transparent, border: none, font: red)
monochrome, plain (bg: transparent, border: none, font: currentColor)
iconOnly, plain (?)

outline (bg: transparent, border: gray, font: black)
destructive, outline (bg: transparent, border: light_red, font: dark_red)
monochrome, outline (bg: transparent, border: currentColor, font: currentColor)

loading (Spinner)

-}

data ButtonType
  = Basic
  | Primary
  | Destructive

  | Plain
  | DestructivePlain
  | MonochromePlain
  | IcononlyPlain

  | Outline
  | DestructiveOutline
  | MonochromeOutline

  | Loading
derive instance eqButtonType :: Eq ButtonType
derive instance ordButtonType :: Ord ButtonType
_button_primary = "Polaris-Button--primary" :: String
_button_destructive = "Polaris-Button--destructive" :: String
_button_plain = "Polaris-Button--plain" :: String
_button_outline = "Polaris-Button--outline" :: String
_button_monochrome = "Polaris-Button--monochrome" :: String
_button_fullWidth = "Polaris-Button--fullWidth" :: String
_button_iconOnly = "Polaris-Button--iconOnly" :: String
_button_loading = "Polaris-Button--loading" :: String
_button_disabled = "Polaris-Button--disabled" :: String

data Size
  = SizeSlim
  | SizeMedium
  | SizeLarge
derive instance eqSize :: Eq Size
derive instance ordSize :: Ord Size
-- NOTE default_size = Medium
_button_sizeSlim = "Polaris-Button--sizeSlim" :: String
_button_sizeLarge = "Polaris-Button--sizeLarge" :: String

data TextAlign
  = TextAlignLeft
  | TextAlignCenter
  | TextAlignRight
derive instance eqTextAlign :: Eq TextAlign
derive instance ordTextAlign :: Ord TextAlign

_button_textAlignLeft = "Polaris-Button--textAlignLeft" :: String
_button_textAlignCenter = "Polaris-Button--textAlignCenter" :: String
_button_textAlignRight = "Polaris-Button--textAlignRight" :: String

type ButtonOptions =
  { buttonType :: ButtonType

  , size :: Size
  , textAlign :: TextAlign
  , fullWidth :: Boolean
  , iconOnly :: Boolean

  , loading :: Boolean
  , isDisabled :: Boolean
  }

defaultOptions :: ButtonOptions
defaultOptions =
  { buttonType: Basic

  , size: SizeMedium
  , textAlign: TextAlignCenter
  , fullWidth: false
  , iconOnly: false

  , loading: false
  , isDisabled: false
  }

buttonClasses :: ButtonOptions -> Array ClassName
buttonClasses { buttonType, size, textAlign, fullWidth, iconOnly, loading, isDisabled}
  = ClassName <$>
    [ _button
    , case size of
        SizeSlim -> _button_sizeSlim
        SizeLarge -> _button_sizeLarge
        SizeMedium -> ""
    , case textAlign of
        TextAlignLeft -> _button_textAlignLeft
        TextAlignCenter -> _button_textAlignCenter
        TextAlignRight -> _button_textAlignRight
    , _button_fullWidth # guard fullWidth
    , _button_iconOnly # guard iconOnly

    , _button_loading # guard loading
    , _button_disabled # guard isDisabled
    ]
    <> case buttonType of
        Basic -> []
        Primary -> [ _button_primary]
        Destructive -> [ _button_destructive ]

        Plain -> [ _button_plain ]
        DestructivePlain -> [ _button_destructive, _button_plain ]
        MonochromePlain -> [ _button_monochrome, _button_plain ]
        IcononlyPlain -> [ _button_iconOnly, _button_plain ]

        Outline -> [ _button_outline ]
        DestructiveOutline -> [ _button_destructive, _button_outline ]
        MonochromeOutline -> [ _button_monochrome, _button_outline ]

        Loading -> [ _button_loading ]

button
  :: forall p i
  . ButtonOptions
  -> Array (IProp HTMLbutton i)
  -> Array (HTML p i)
  -> HTML p i
button opts iprops inner = blockBuilder HH.button (buttonClasses opts) iprops
  [ buttonContent_ inner ]
  where
    buttonContent_ = HH.span [ HP.class_ $ ClassName _buttonContent ]

button_
  :: forall p i
  . ButtonOptions
  -> Array (HTML p i)
  -> HTML p i
button_ opts = button opts []

text
  :: forall p i
  . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
text = blockBuilder HH.span $ ClassName <$> [ _buttonText ]

text_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
text_ = text []

icon
  :: forall p i
  . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
icon = blockBuilder HH.span $ ClassName <$> [ _buttonIcon ]

icon_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
icon_ = icon []

spinner
  :: forall p i
  . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
spinner = blockBuilder HH.span $ ClassName <$> [ _buttonSpinner ]

spinner_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
spinner_ = spinner []

