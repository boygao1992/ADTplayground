module Polaris.UI.Block.Banner where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
-- import Ocelot.Block.Builder (blockBuilder)
import Polaris.UI.Block.Button as Button
-- import Polaris.UI.Block.ButtonGroup as ButtonGroup
import Polaris.UI.Block.Heading as Heading
import Polaris.UI.Block.Icon as Icon
import Polaris.UI.Block.Icon.Color as IconColor
import Polaris.UI.Block.Icon.Svg as IconSvg

_banner = "Polaris-Banner" :: String
_banner_hasDismiss = "Polaris-Banner--hasDismiss" :: String

data Env
  = WithinContentContainer
  | WithinPage
derive instance eqEnv :: Eq Env
derive instance ordEnv :: Ord Env
_banner_withinContentContainer = "Polaris-Banner--withinContentContainer" :: String
_banner_withinPage = "Polaris-Banner--withinPage" :: String

envToClassName :: Env -> ClassName
envToClassName env = ClassName case env of
  WithinContentContainer -> _banner_withinContentContainer
  WithinPage -> _banner_withinPage

data Status
  = Basic
  | Success
  | Info
  | Warning
  | Critical
derive instance eqStatus :: Eq Status
derive instance ordStatus :: Ord Status
_banner_statusSuccess = "Polaris-Banner--statusSuccess" :: String
_banner_statusInfo = "Polaris-Banner--statusInfo" :: String
_banner_statusWarning = "Polaris-Banner--statusWarning" :: String
_banner_statusCritical = "Polaris-Banner--statusCritical" :: String

statusToClassNames :: Status -> Array ClassName
statusToClassNames status = ClassName <$> case status of
  Success -> [ _banner_statusSuccess ]
  Info -> [ _banner_statusInfo ]
  Warning -> [ _banner_statusWarning ]
  Critical -> [ _banner_statusCritical ]
  Basic -> []

statusToIcon :: forall p i. Status -> HTML p i
statusToIcon = case _ of
  Success ->
    Icon.iconSvg_ { color: Just IconColor.GreenDark, backdrop: true }
    [ IconSvg._CircleTickMajorTwotone
    ]
  Info ->
    Icon.iconSvg_ { color: Just IconColor.TealDark, backdrop: true }
    [ IconSvg._CircleInformationMajorTwotone
    ]
  Warning ->
    Icon.iconSvg_ { color: Just IconColor.YellowDark, backdrop: true }
    [ IconSvg._CircleAlertMajorTwotone
    ]
  Critical ->
    Icon.iconSvg_ { color: Just IconColor.RedDark, backdrop: true }
    [ IconSvg._CircleDisabledMajorTwotone
    ]
  Basic ->
    Icon.iconSvg_ { color: Just IconColor.InkLighter, backdrop: true }
    [ IconSvg._FlagMajorTwotone
    ]

_bannerHeading = "Polaris-Banner__Heading" :: String
_bannerContent = "Polaris-Banner__Content" :: String
_bannerRibbon = "Polaris-Banner__Ribbon" :: String
_bannerPrimaryAction = "Polaris-Banner__PrimaryAction" :: String
_bannerSecondaryAction = "Polaris-Banner__SecondaryAction" :: String
_bannerText = "Polaris-Banner__Text" :: String
_bannerDismiss = "Polaris-Banner__Dismiss" :: String

dismissButton
  :: forall p i
  . Array (IProp HTMLbutton i)
  -> HTML p i
dismissButton iprops =
  Button.button Button.defaultOptions { buttonType = Button.Plain }
                ([ HP.class_ $ ClassName _bannerDismiss ] <> iprops)
  [ Button.icon_
    [ Icon.iconSvg_ Icon.defaultOptions
      [ IconSvg.cancelSmallMinor
      ]
    ]
  ]

heading :: forall p i. String -> HTML p i
heading str =
  HH.div [ HP.class_ $ ClassName _bannerHeading ]
  [ Heading.headingP_
    [ HH.text str ]
  ]

ribbon :: forall i p. Status -> HTML p i
ribbon status =
  HH.div [ HP.class_ $ ClassName _bannerRibbon ]
  [ statusToIcon status
  ]

content :: forall p i. Array (HTML p i) -> HTML p i
content = HH.div [ HP.class_ $ ClassName _bannerContent]

primaryAction
  :: forall i p
  . Env
  -> Array (IProp HTMLbutton i)
  -> Array (HTML p i)
  -> HTML p i
primaryAction env iprops inner =
  HH.div [ HP.class_ $ ClassName _bannerPrimaryAction ]
  [ Button.button Button.defaultOptions
                  { buttonType = Button.Outline
                  , size = case env of
                      WithinContentContainer -> Button.SizeSlim
                      WithinPage -> Button.SizeMedium
                  }
                  iprops
    inner
  ]

-- TODO
-- secondaryAction =
