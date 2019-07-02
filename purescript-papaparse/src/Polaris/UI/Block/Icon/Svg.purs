module Polaris.UI.Block.Icon.Svg where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Int (toNumber) as Int
import Halogen.HTML as HH
import Svg.Attributes as SA
import Svg.Elements as SE
import Polaris.UI.Block.Icon (_iconSvg)

type IconSvgCircle =
  { cx :: Int
  , cy :: Int
  , r :: Int
  , fill :: Boolean
  , fillRule :: Boolean
  }

shopifyIconSvgCircle :: forall p i. IconSvgCircle -> HH.HTML p i
shopifyIconSvgCircle { cx, cy, r, fill, fillRule } =
  SE.circle $
  [ SA.cx $ Int.toNumber cx
  , SA.cy $ Int.toNumber cy
  , SA.r $ Int.toNumber r
  ]
  <> if fill
     then [ SA.fill $ Just SA.CurrentColor ]
     else []
  <> if fillRule
      then [ SA.fillRule SA.EvenOdd ]
      else []

type IconSvgPath =
  { fill :: Boolean
  , fillRule :: Boolean
  , d :: String
  }

shopifyIconSvgPath :: forall p i. IconSvgPath -> HH.HTML p i
shopifyIconSvgPath { fill, fillRule, d } =
  SE.path
  $ [ SA.unsafeD d ]
  <> if fill
    then [ SA.fill $ Just SA.CurrentColor ]
    else []
  <> if fillRule
    then [ SA.fillRule SA.EvenOdd ]
    else []

shopifyIconContainer :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
shopifyIconContainer =
  SE.svg  [ SA.viewBox 0.0 0.0 20.0 20.0
          , SA.class_ _iconSvg
          ]

shopifyIconSvgCirclePath :: forall p i. IconSvgCircle -> IconSvgPath -> HH.HTML p i
shopifyIconSvgCirclePath circle path =
  shopifyIconContainer
  [ shopifyIconSvgCircle circle
  , shopifyIconSvgPath path
  ]

shopifyIconSvgDoublePaths :: forall p i. IconSvgPath -> IconSvgPath -> HH.HTML p i
shopifyIconSvgDoublePaths path1 path2 =
  shopifyIconContainer
  [ shopifyIconSvgPath path1
  , shopifyIconSvgPath path2
  ]

shopifyIconSvgFromString :: forall p i. String -> HH.HTML p i
shopifyIconSvgFromString pathStr =
  shopifyIconContainer
  [ SE.path [ SA.unsafeD pathStr
            , SA.fillRule SA.EvenOdd
            ]
  ]

-- | Major

-- CircleTickMajorTwotone

_ref__2j =
  {
  fill: true,
  cx: 10,
  cy: 10,
  r: 9
  , fillRule: false
  } :: IconSvgCircle

_ref2__H =
  {
  d: "M10 0C4.486 0 0 4.486 0 10s4.486 10 10 10 10-4.486 10-10S15.514 0 10 0m0 18c-4.41 0-8-3.59-8-8s3.59-8 8-8 8 3.59 8 8-3.59 8-8 8m2.293-10.707L9 10.586 7.707 9.293a1 1 0 1 0-1.414 1.414l2 2a.997.997 0 0 0 1.414 0l4-4a1 1 0 1 0-1.414-1.414",
  fill: false
  , fillRule: false
  } :: IconSvgPath

_CircleTickMajorTwotone :: forall p i. HH.HTML p i
_CircleTickMajorTwotone = shopifyIconSvgCirclePath _ref__2j _ref2__H

_ref__3P =
  {
  fill: true,
  d: "M2 3h11v4h6l-2 4 2 4H8v-4H3"
  , fillRule: false
  } :: IconSvgPath

_ref2__1d =
  {
  d: "M16.105 11.447L17.381 14H9v-2h4a1 1 0 0 0 1-1V8h3.38l-1.274 2.552a.993.993 0 0 0 0 .895zM2.69 4H12v6H4.027L2.692 4zm15.43 7l1.774-3.553A1 1 0 0 0 19 6h-5V3c0-.554-.447-1-1-1H2.248L1.976.782a1 1 0 1 0-1.953.434l4 18a1.006 1.006 0 0 0 1.193.76 1 1 0 0 0 .76-1.194L4.47 12H7v3a1 1 0 0 0 1 1h11c.346 0 .67-.18.85-.476a.993.993 0 0 0 .044-.972l-1.775-3.553z",
  fill: false
  , fillRule: false
  } :: IconSvgPath

_FlagMajorTwotone :: forall p i. HH.HTML p i
_FlagMajorTwotone = shopifyIconSvgDoublePaths _ref__3P _ref2__1d

_ref__1R =
  {
  fill: true,
  cx: 10,
  cy: 10,
  r: 9
  , fillRule: false
  } :: IconSvgCircle

_ref2__w =
  {
  d: "M10 0C4.486 0 0 4.486 0 10s4.486 10 10 10 10-4.486 10-10S15.514 0 10 0m0 18c-4.41 0-8-3.59-8-8s3.59-8 8-8 8 3.59 8 8-3.59 8-8 8m0-13a1 1 0 0 0-1 1v4a1 1 0 1 0 2 0V6a1 1 0 0 0-1-1m0 8a1 1 0 1 0 0 2 1 1 0 0 0 0-2",
  fill: false
  , fillRule: false
  } :: IconSvgPath

_CircleAlertMajorTwotone :: forall p i. HH.HTML p i
_CircleAlertMajorTwotone = shopifyIconSvgCirclePath _ref__1R  _ref2__w

_ref__1__ =
  {
  fill: true,
  cx: 10,
  cy: 10,
  r: 9
  , fillRule: false
  } :: IconSvgCircle

_ref2__y =
  {
  d: "M2 10c0-1.846.635-3.543 1.688-4.897l11.209 11.209A7.954 7.954 0 0 1 10 18c-4.411 0-8-3.589-8-8m14.312 4.897L5.103 3.688A7.954 7.954 0 0 1 10 2c4.411 0 8 3.589 8 8a7.952 7.952 0 0 1-1.688 4.897M0 10c0 5.514 4.486 10 10 10s10-4.486 10-10S15.514 0 10 0 0 4.486 0 10",
  fill: false
  , fillRule: false
  } :: IconSvgPath

_CircleDisabledMajorTwotone :: forall p i. HH.HTML p i
_CircleDisabledMajorTwotone =  shopifyIconSvgCirclePath _ref__1__ _ref2__y

_ref__25 =
  {
  cx: 10,
  cy: 10,
  r: 9,
  fill: true
  , fillRule: false
  } :: IconSvgCircle

_ref2__B =
  {
  fill: false,
  d: "M10 0C4.486 0 0 4.486 0 10s4.486 10 10 10 10-4.486 10-10S15.514 0 10 0m0 18c-4.411 0-8-3.589-8-8s3.589-8 8-8 8 3.589 8 8-3.589 8-8 8m1-5v-3a1 1 0 0 0-1-1H9a1 1 0 1 0 0 2v3a1 1 0 0 0 1 1h1a1 1 0 1 0 0-2m-1-5.9a1.1 1.1 0 1 0 0-2.2 1.1 1.1 0 0 0 0 2.2"
  , fillRule: false
  } :: IconSvgPath

_CircleInformationMajorTwotone :: forall p i. HH.HTML p i
_CircleInformationMajorTwotone = shopifyIconSvgCirclePath _ref__25 _ref2__B

-- | Minor

plusMinor :: forall p i. HH.HTML p i
plusMinor = shopifyIconSvgFromString "M17 9h-6V3a1 1 0 1 0-2 0v6H3a1 1 0 1 0 0 2h6v6a1 1 0 1 0 2 0v-6h6a1 1 0 1 0 0-2"

alertMinor :: forall p i. HH.HTML p i
alertMinor = shopifyIconSvgFromString "M10 18a8 8 0 1 1 0-16 8 8 0 0 1 0 16zm-1-8h2V6H9v4zm0 4h2v-2H9v2z"

arrowDownMinor :: forall p i. HH.HTML p i
arrowDownMinor = shopifyIconSvgFromString "M10.707 17.707l5-5a.999.999 0 1 0-1.414-1.414L11 14.586V3a1 1 0 1 0-2 0v11.586l-3.293-3.293a.999.999 0 1 0-1.414 1.414l5 5a.999.999 0 0 0 1.414 0"

arrowLeftMinor :: forall p i. HH.HTML p i
arrowLeftMinor = shopifyIconSvgFromString "M17 9H5.414l3.293-3.293a.999.999 0 1 0-1.414-1.414l-5 5a.999.999 0 0 0 0 1.414l5 5a.997.997 0 0 0 1.414 0 .999.999 0 0 0 0-1.414L5.414 11H17a1 1 0 1 0 0-2"

arrowRightMinor :: forall p i. HH.HTML p i
arrowRightMinor = shopifyIconSvgFromString "M17.707 9.293l-5-5a.999.999 0 1 0-1.414 1.414L14.586 9H3a1 1 0 1 0 0 2h11.586l-3.293 3.293a.999.999 0 1 0 1.414 1.414l5-5a.999.999 0 0 0 0-1.414"

arrowUpMinor :: forall p i. HH.HTML p i
arrowUpMinor = shopifyIconSvgFromString "M11 17V5.414l3.293 3.293a.999.999 0 1 0 1.414-1.414l-5-5a.999.999 0 0 0-1.414 0l-5 5a.997.997 0 0 0 0 1.414.999.999 0 0 0 1.414 0L9 5.414V17a1 1 0 1 0 2 0"

arrowUpDownMinor :: forall p i. HH.HTML p i -- NOTE ArrowUpDownMinor = SvgSelectMinor
arrowUpDownMinor = shopifyIconSvgFromString "M13 8l-3-3-3 3h6zm-.1 4L10 14.9 7.1 12h5.8z"

calendarMinor :: forall p i. HH.HTML p i
calendarMinor = shopifyIconSvgFromString "M4 8h12V6H4v2zm9 4h2v-2h-2v2zm-4 0h2v-2H9v2zm0 4h2v-2H9v2zm-4-4h2v-2H5v2zm0 4h2v-2H5v2zM17 4h-2V3a1 1 0 1 0-2 0v1H7V3a1 1 0 1 0-2 0v1H3a1 1 0 0 0-1 1v12a1 1 0 0 0 1 1h14a1 1 0 0 0 1-1V5a1 1 0 0 0-1-1z"

mobileCancelMajorMonotone :: forall p i. HH.HTML p i
mobileCancelMajorMonotone = shopifyIconSvgFromString "M11.414 10l6.293-6.293a.999.999 0 1 0-1.414-1.414L10 8.586 3.707 2.293a.999.999 0 1 0-1.414 1.414L8.586 10l-6.293 6.293a.999.999 0 1 0 1.414 1.414L10 11.414l6.293 6.293a.997.997 0 0 0 1.414 0 .999.999 0 0 0 0-1.414L11.414 10z"

cancelSmallMinor :: forall p i. HH.HTML p i
cancelSmallMinor = shopifyIconSvgFromString "M11.414 10l4.293-4.293a.999.999 0 1 0-1.414-1.414L10 8.586 5.707 4.293a.999.999 0 1 0-1.414 1.414L8.586 10l-4.293 4.293a.999.999 0 1 0 1.414 1.414L10 11.414l4.293 4.293a.997.997 0 0 0 1.414 0 .999.999 0 0 0 0-1.414L11.414 10z"

caretDownMinor :: forall p i. HH.HTML p i
caretDownMinor = shopifyIconSvgFromString "M5 8l5 5 5-5z"

caretUpMinor :: forall p i. HH.HTML p i
caretUpMinor = shopifyIconSvgFromString "M15 12l-5-5-5 5z"

tickSmallMinor :: forall p i. HH.HTML p i
tickSmallMinor = shopifyIconSvgFromString "M8.315 13.859l-3.182-3.417a.506.506 0 0 1 0-.684l.643-.683a.437.437 0 0 1 .642 0l2.22 2.393 4.942-5.327a.437.437 0 0 1 .643 0l.643.684a.504.504 0 0 1 0 .683l-5.91 6.35a.437.437 0 0 1-.642 0"

chevronDownMinor :: forall p i. HH.HTML p i
chevronDownMinor = shopifyIconSvgFromString "M10 14a.997.997 0 0 1-.707-.293l-5-5a.999.999 0 1 1 1.414-1.414L10 11.586l4.293-4.293a.999.999 0 1 1 1.414 1.414l-5 5A.997.997 0 0 1 10 14"

chevronLeftMinor :: forall p i. HH.HTML p i
chevronLeftMinor = shopifyIconSvgFromString "M12 16a.997.997 0 0 1-.707-.293l-5-5a.999.999 0 0 1 0-1.414l5-5a.999.999 0 1 1 1.414 1.414L8.414 10l4.293 4.293A.999.999 0 0 1 12 16"
chevronRightMinor :: forall p i. HH.HTML p i
chevronRightMinor = shopifyIconSvgFromString "M8 16a.999.999 0 0 1-.707-1.707L11.586 10 7.293 5.707a.999.999 0 1 1 1.414-1.414l5 5a.999.999 0 0 1 0 1.414l-5 5A.997.997 0 0 1 8 16"

chevronUpMinor :: forall p i. HH.HTML p i
chevronUpMinor = shopifyIconSvgFromString "M15 13a.997.997 0 0 1-.707-.293L10 8.414l-4.293 4.293a.999.999 0 1 1-1.414-1.414l5-5a.999.999 0 0 1 1.414 0l5 5A.999.999 0 0 1 15 13"

circleCancelMinor :: forall p i. HH.HTML p i
circleCancelMinor = shopifyIconSvgFromString "M14.242 12.829l-1.414 1.414L10 11.413l-2.828 2.83-1.414-1.414 2.828-2.83-2.828-2.827 1.414-1.414L10 8.586l2.828-2.828 1.414 1.414L11.414 10l2.828 2.829zM10 1.999A8 8 0 1 0 10 18a8 8 0 0 0 0-16z"

circleChevronDownMinor :: forall p i. HH.HTML p i
circleChevronDownMinor = shopifyIconSvgFromString "M10 13.414L5.293 8.707l1.414-1.414L10 10.586l3.293-3.293 1.414 1.414L10 13.414zM10 2a8 8 0 1 0 0 16 8 8 0 0 0 0-16z"

circleChevronLeftMinor :: forall p i. HH.HTML p i
circleChevronLeftMinor = shopifyIconSvgFromString "M11.293 5.293l1.414 1.414L9.414 10l3.293 3.293-1.414 1.414L6.586 10l4.707-4.707zM10 2a8 8 0 1 0 0 16 8 8 0 0 0 0-16z"

circleChevronRightMinor :: forall p i. HH.HTML p i
circleChevronRightMinor = shopifyIconSvgFromString "M8.707 14.707l-1.414-1.414L10.586 10 7.293 6.707l1.414-1.414L13.414 10l-4.707 4.707zM10 18a8 8 0 1 0 0-16 8 8 0 0 0 0 16z"

circleChevronUpMinor :: forall p i. HH.HTML p i
circleChevronUpMinor = shopifyIconSvgFromString "M14.707 11.293l-1.414 1.414L10 9.414l-3.293 3.293-1.414-1.414L10 6.586l4.707 4.707zM18 10a8 8 0 1 0-16 0 8 8 0 0 0 16 0z"

circleInformationMajorTwotone :: forall p i. HH.HTML p i
circleInformationMajorTwotone = shopifyIconSvgFromString "M10 0C4.486 0 0 4.486 0 10s4.486 10 10 10 10-4.486 10-10S15.514 0 10 0m0 18c-4.411 0-8-3.589-8-8s3.589-8 8-8 8 3.589 8 8-3.589 8-8 8m1-5v-3a1 1 0 0 0-1-1H9a1 1 0 1 0 0 2v3a1 1 0 0 0 1 1h1a1 1 0 1 0 0-2m-1-5.9a1.1 1.1 0 1 0 0-2.2 1.1 1.1 0 0 0 0 2.2"

circlePlusMinor :: forall p i. HH.HTML p i
circlePlusMinor = shopifyIconSvgFromString "M15 11h-4v4H9v-4H5V9h4V5h2v4h4v2zm-5-9a8 8 0 1 0 0 16 8 8 0 0 0 0-16z"

circlePlusOutlineMinor :: forall p i. HH.HTML p i
circlePlusOutlineMinor = shopifyIconSvgFromString "M10 2c-4.411 0-8 3.589-8 8s3.589 8 8 8 8-3.589 8-8-3.589-8-8-8zm0 14c-3.309 0-6-2.691-6-6s2.691-6 6-6 6 2.691 6 6-2.691 6-6 6zm3-7h-2V7a1 1 0 1 0-2 0v2H7a1 1 0 1 0 0 2h2v2a1 1 0 1 0 2 0v-2h2a1 1 0 1 0 0-2z"

conversationMinor :: forall p i. HH.HTML p i
conversationMinor = shopifyIconSvgFromString "M13 11h2V9h-2v2zm-4 0h2V9H9v2zm-4 0h2V9H5v2zm5-9c-4.411 0-8 3.589-8 8 0 1.504.425 2.908 1.15 4.111l-1.069 2.495a1 1 0 0 0 1.314 1.313l2.494-1.069A7.939 7.939 0 0 0 10 18c4.411 0 8-3.589 8-8s-3.589-8-8-8z"

deleteMinor :: forall p i. HH.HTML p i
deleteMinor = shopifyIconSvgFromString "M16 6H4a1 1 0 1 0 0 2h1v9a1 1 0 0 0 1 1h8a1 1 0 0 0 1-1V8h1a1 1 0 1 0 0-2zM9 4a1 1 0 1 1 0-2h2a1 1 0 1 1 0 2H9zm2 12h2V8h-2v8zm-4 0h2V8H7v8z"

circleDisableMinor :: forall p i. HH.HTML p i
circleDisableMinor = shopifyIconSvgFromString "M10 16a5.961 5.961 0 0 1-3.471-1.115l8.356-8.356A5.961 5.961 0 0 1 16 10c0 3.309-2.691 6-6 6m0-12c1.294 0 2.49.416 3.471 1.115l-8.356 8.356A5.961 5.961 0 0 1 4 10c0-3.309 2.691-6 6-6m0-2c-4.411 0-8 3.589-8 8s3.589 8 8 8 8-3.589 8-8-3.589-8-8-8"

disputeMinor :: forall p i. HH.HTML p i
disputeMinor = shopifyIconSvgFromString "M9 10h2V6H9v4zm0 4h2v-2H9v2zm-7-4c0 4.411 3.589 8 8 8a7.939 7.939 0 0 0 4.111-1.15l2.494 1.069a1 1 0 0 0 1.314-1.313l-1.069-2.495A7.939 7.939 0 0 0 18 10c0-4.411-3.589-8-8-8s-8 3.589-8 8z"

duplicateMinor :: forall p i. HH.HTML p i
duplicateMinor = shopifyIconSvgFromString "M8 12h8V4H8v8zm4 4H4V8h2v5a1 1 0 0 0 1 1h5v2zm5-14H7a1 1 0 0 0-1 1v3H3a1 1 0 0 0-1 1v10a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1v-3h3a1 1 0 0 0 1-1V3a1 1 0 0 0-1-1z"

embedMinor :: forall p i. HH.HTML p i
embedMinor = shopifyIconSvgFromString "M17 13a1 1 0 0 1 1 1v3a1 1 0 0 1-1 1H3a1 1 0 0 1-1-1v-3a1 1 0 1 1 2 0v2h12v-2a1 1 0 0 1 1-1zm0-11a1 1 0 0 1 1 1v3a1 1 0 1 1-2 0V4H4v2a1 1 0 1 1-2 0V3a1 1 0 0 1 1-1h14zm.555 7.168a1.001 1.001 0 0 1 0 1.664l-3 2a1 1 0 0 1-1.109-1.664L15.198 10l-1.752-1.168a1 1 0 1 1 1.109-1.664l3 2zM6.832 7.445a1 1 0 0 1-.277 1.387L4.803 10l1.752 1.168a1 1 0 1 1-1.11 1.664l-3-2a1.001 1.001 0 0 1 0-1.664l3-2a1 1 0 0 1 1.387.277zM9 14.001a1 1 0 0 1-.948-1.317l2-6a1 1 0 0 1 1.896.633l-2 6A.999.999 0 0 1 9 14z"

exportMinor :: forall p i. HH.HTML p i
exportMinor = shopifyIconSvgFromString "M13.707 6.707a.997.997 0 0 1-1.414 0L11 5.414V13a1 1 0 1 1-2 0V5.414L7.707 6.707a.999.999 0 1 1-1.414-1.414l3-3a.999.999 0 0 1 1.414 0l3 3a.999.999 0 0 1 0 1.414zM17 18H3a1 1 0 1 1 0-2h14a1 1 0 1 1 0 2z"

externalMinor :: forall p i. HH.HTML p i
externalMinor = shopifyIconSvgFromString "M17 2a1 1 0 0 1 1 1v4a1 1 0 1 1-2 0V5.414l-7.293 7.293a.997.997 0 0 1-1.414 0 .999.999 0 0 1 0-1.414L14.586 4H13a1 1 0 1 1 0-2h4zm-4 9a1 1 0 0 1 1 1v5a1 1 0 0 1-1 1H3a1 1 0 0 1-1-1V7a1 1 0 0 1 1-1h5a1 1 0 1 1 0 2H4v8h8v-4a1 1 0 0 1 1-1z"

questionMarkMajorTwotone :: forall p i. HH.HTML p i
questionMarkMajorTwotone = shopifyIconSvgFromString "M10 0C4.486 0 0 4.486 0 10s4.486 10 10 10 10-4.486 10-10S15.514 0 10 0m0 18c-4.411 0-8-3.589-8-8s3.589-8 8-8 8 3.589 8 8-3.589 8-8 8m0-4a1 1 0 1 0 0 2 1 1 0 1 0 0-2m0-10C8.346 4 7 5.346 7 7a1 1 0 1 0 2 0 1.001 1.001 0 1 1 1.591.808C9.58 8.548 9 9.616 9 10.737V11a1 1 0 1 0 2 0v-.263c0-.653.484-1.105.773-1.317A3.013 3.013 0 0 0 13 7c0-1.654-1.346-3-3-3"

homeMajorMonotone :: forall p i. HH.HTML p i
homeMajorMonotone = shopifyIconSvgFromString "M19.664 8.252l-9-8a1 1 0 0 0-1.328 0L8 1.44V1a1 1 0 0 0-1-1H3a1 1 0 0 0-1 1v5.773L.336 8.252a1.001 1.001 0 0 0 1.328 1.496L2 9.449V19a1 1 0 0 0 1 1h14a1 1 0 0 0 1-1V9.449l.336.299a.997.997 0 0 0 1.411-.083 1.001 1.001 0 0 0-.083-1.413zM16 18h-2v-5a1 1 0 0 0-1-1H7a1 1 0 0 0-1 1v5H4V7.671l6-5.333 6 5.333V18zm-8 0v-4h4v4H8zM4 2h2v1.218L4 4.996V2z"

horizontalDotsMinor :: forall p i. HH.HTML p i
horizontalDotsMinor = shopifyIconSvgFromString "M6 10a2 2 0 1 1-4.001-.001A2 2 0 0 1 6 10zm6 0a2 2 0 1 1-4.001-.001A2 2 0 0 1 12 10zm6 0a2 2 0 1 1-4.001-.001A2 2 0 0 1 18 10z"

importMinor :: forall p i. HH.HTML p i
importMinor = shopifyIconSvgFromString "M9.293 13.707l-3-3a.999.999 0 1 1 1.414-1.414L9 10.586V3a1 1 0 1 1 2 0v7.586l1.293-1.293a.999.999 0 1 1 1.414 1.414l-3 3a.999.999 0 0 1-1.414 0zM17 16a1 1 0 1 1 0 2H3a1 1 0 1 1 0-2h14z"

logOutMinor :: forall p i. HH.HTML p i
logOutMinor = shopifyIconSvgFromString "M10 16a1 1 0 1 1 0 2c-4.411 0-8-3.589-8-8s3.589-8 8-8a1 1 0 1 1 0 2c-3.309 0-6 2.691-6 6s2.691 6 6 6zm7.707-6.707a.999.999 0 0 1 0 1.414l-3 3a.997.997 0 0 1-1.414 0 .999.999 0 0 1 0-1.414L14.586 11H10a1 1 0 1 1 0-2h4.586l-1.293-1.293a.999.999 0 1 1 1.414-1.414l3 3z"

mobileHamburgerMajorMonotone :: forall p i. HH.HTML p i
mobileHamburgerMajorMonotone = shopifyIconSvgFromString "M19 11H1a1 1 0 1 1 0-2h18a1 1 0 1 1 0 2zm0-7H1a1 1 0 0 1 0-2h18a1 1 0 1 1 0 2zm0 14H1a1 1 0 0 1 0-2h18a1 1 0 1 1 0 2z"

noteMinor :: forall p i. HH.HTML p i
noteMinor = shopifyIconSvgFromString "M6 11h8V9H6v2zm0 4h8v-2H6v2zm0-8h4V5H6v2zm9.707-1.707l-3-3A.996.996 0 0 0 12 2H5a1 1 0 0 0-1 1v14a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1V6a.997.997 0 0 0-.293-.707z"

notificationMajorMonotone :: forall p i. HH.HTML p i
notificationMajorMonotone = shopifyIconSvgFromString "M16 8c0-2.967-2.167-5.432-5-5.91V1a1 1 0 1 0-2 0v1.09C6.167 2.568 4 5.033 4 8c0 2.957 0 4.586-1.707 6.293A1 1 0 0 0 3 16h4.183A2.909 2.909 0 0 0 7 17c0 1.654 1.345 3 3 3s3-1.346 3-3c0-.353-.07-.687-.184-1H17a1 1 0 0 0 .707-1.707C16 12.586 16 10.957 16 8zM5.011 14C6 12.208 6 10.285 6 8c0-2.206 1.794-4 4-4s4 1.794 4 4c0 2.285 0 4.208.989 6H5.011zM11 17a1.001 1.001 0 0 1-2 0 1 1 0 0 1 2 0z"

onlineStoreMajorTwotone :: forall p i. HH.HTML p i
onlineStoreMajorTwotone = shopifyIconSvgFromString "M16 8c-1.103 0-2-.897-2-2h4c0 1.103-.897 2-2 2zm0 6H4v-4a3.99 3.99 0 0 0 3-1.357A3.99 3.99 0 0 0 10 10a3.99 3.99 0 0 0 3-1.357A3.99 3.99 0 0 0 16 10v4zm-3.281 4H7.281c.357-.702.536-1.434.627-2h4.184c.091.566.27 1.298.627 2zM12 6c0 1.103-.897 2-2 2s-2-.897-2-2h4zM2 6h4c0 1.103-.897 2-2 2s-2-.897-2-2zm1.618-4h12.764l1 2H2.618l1-2zm16.277 2.553l-2-4A1.001 1.001 0 0 0 17 0H3c-.379 0-.725.214-.895.553l-2 4A1.002 1.002 0 0 0 0 5v1c0 1.474.811 2.75 2 3.444V15a1 1 0 0 0 1 1h2.871c-.157.747-.508 1.7-1.318 2.105a1.002 1.002 0 0 0-.527 1.125c.108.451.51.77.974.77h10c.464 0 .866-.319.974-.77a1.002 1.002 0 0 0-.527-1.125c-.801-.4-1.153-1.356-1.313-2.105H17a1 1 0 0 0 1-1V9.444c1.189-.694 2-1.97 2-3.444V5c0-.155-.036-.309-.105-.447z"

ordersMajorTwotone :: forall p i. HH.HTML p i
ordersMajorTwotone = shopifyIconSvgFromString "M2 18v-4h3.382l.723 1.447c.17.339.516.553.895.553h6c.379 0 .725-.214.895-.553L14.618 14H18v4H2zM19 1a1 1 0 0 1 1 1v17a1 1 0 0 1-1 1H1a1 1 0 0 1-1-1V2a1 1 0 0 1 1-1h4a1 1 0 0 1 0 2H2v9h4c.379 0 .725.214.895.553L7.618 14h4.764l.723-1.447c.17-.339.516-.553.895-.553h4V3h-3a1 1 0 0 1 0-2h4zM6.293 6.707a.999.999 0 1 1 1.414-1.414L9 6.586V1a1 1 0 0 1 2 0v5.586l1.293-1.293a.999.999 0 1 1 1.414 1.414l-3 3a.997.997 0 0 1-1.414 0l-3-3z"

printMinor :: forall p i. HH.HTML p i
printMinor = shopifyIconSvgFromString "M14 11h2V9h-2v2zM7 7h6V4H7v3zm0 9h6v-2H7v2zm10-9h-2V3a1 1 0 0 0-1-1H6a1 1 0 0 0-1 1v4H3a1 1 0 0 0-1 1v7a1 1 0 0 0 1 1h2v1a1 1 0 0 0 1 1h8a1 1 0 0 0 1-1v-1h2a1 1 0 0 0 1-1V8a1 1 0 0 0-1-1z"

productsMajorTwotone :: forall p i. HH.HTML p i
productsMajorTwotone = shopifyIconSvgFromString "M19 0h-9c-.265 0-.52.106-.707.293l-9 9a.999.999 0 0 0 0 1.414l9 9a.997.997 0 0 0 1.414 0l9-9A.997.997 0 0 0 20 10V1a1 1 0 0 0-1-1zm-9 17.586L2.414 10 4 8.414 11.586 16 10 17.586zm8-8l-5 5L5.414 7l5-5H18v7.586zM15 6a1 1 0 1 0 0-2 1 1 0 0 0 0 2"

profileMinor :: forall p i. HH.HTML p i
profileMinor = shopifyIconSvgFromString "M10 2c4.411 0 8 3.589 8 8s-3.589 8-8 8-8-3.589-8-8 3.589-8 8-8zm0 3c-1.104 0-2 .897-2 2s.896 2 2 2 2-.897 2-2-.896-2-2-2zm0 10c1.631 0 3.064-.792 3.978-2-.914-1.208-2.347-2-3.978-2-1.631 0-3.064.792-3.978 2 .914 1.208 2.347 2 3.978 2z"

refreshMinor :: forall p i. HH.HTML p i
refreshMinor = shopifyIconSvgFromString "M17 11a1 1 0 0 1 1 1c0 1.654-1.346 3-3 3H5.414l1.293 1.293a.999.999 0 1 1-1.414 1.414l-3-3a.999.999 0 0 1 0-1.414l3-3a.999.999 0 1 1 1.414 1.414L5.414 13H15c.552 0 1-.449 1-1a1 1 0 0 1 1-1zM3 9a1 1 0 0 1-1-1c0-1.654 1.346-3 3-3h9.586l-1.293-1.293a.999.999 0 1 1 1.414-1.414l3 3a.999.999 0 0 1 0 1.414l-3 3a.997.997 0 0 1-1.414 0 .999.999 0 0 1 0-1.414L14.586 7H5c-.552 0-1 .449-1 1a1 1 0 0 1-1 1z"

riskMinor :: forall p i. HH.HTML p i
riskMinor = shopifyIconSvgFromString "M9 12h2V8H9v4zm0 4h2v-2H9v2zm8.895.509l-7-14c-.339-.678-1.451-.678-1.79 0l-7 14A.999.999 0 0 0 3 17.956h14a1.001 1.001 0 0 0 .895-1.447z"

saveMinor :: forall p i. HH.HTML p i
saveMinor = shopifyIconSvgFromString "M17 4h-3a1 1 0 1 0 0 2h2v10H4V4h3.586L9 5.414v5.172L7.707 9.293a1 1 0 0 0-1.414 1.414l3 3a.996.996 0 0 0 1.414 0l3-3a1 1 0 0 0-1.414-1.414L11 10.586V5a.997.997 0 0 0-.293-.707l-2-2A.994.994 0 0 0 8 2H3a1 1 0 0 0-1 1v14a1 1 0 0 0 1 1h14a1 1 0 0 0 1-1V5a1 1 0 0 0-1-1z"

searchMinor :: forall p i. HH.HTML p i
searchMinor = shopifyIconSvgFromString "M8 12a4 4 0 1 1 0-8 4 4 0 0 1 0 8m9.707 4.293l-4.82-4.82A5.968 5.968 0 0 0 14 8 6 6 0 0 0 2 8a6 6 0 0 0 6 6 5.968 5.968 0 0 0 3.473-1.113l4.82 4.82a.997.997 0 0 0 1.414 0 .999.999 0 0 0 0-1.414"

minusMinor :: forall p i. HH.HTML p i
minusMinor = shopifyIconSvgFromString "M15 9H5a1 1 0 1 0 0 2h10a1 1 0 1 0 0-2"

viewMinor :: forall p i. HH.HTML p i
viewMinor = shopifyIconSvgFromString "M17.928 9.628C17.836 9.399 15.611 4 9.999 4S2.162 9.399 2.07 9.628a1.017 1.017 0 0 0 0 .744C2.162 10.601 4.387 16 9.999 16s7.837-5.399 7.929-5.628a1.017 1.017 0 0 0 0-.744zM9.999 14a4 4 0 1 1 0-8 4 4 0 0 1 0 8zm0-6A2 2 0 1 0 10 12.001 2 2 0 0 0 10 8z"
