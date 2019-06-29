module Polaris.UI.PageSvg where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Polaris.UI.Block.Icon (iconSvg)
import Polaris.UI.Block.Icon.Color as Color
import Polaris.UI.Block.Icon.Svg (alertMinor, arrowDownMinor, arrowLeftMinor, arrowRightMinor, arrowUpDownMinor, arrowUpMinor, calendarMinor, cancelSmallMinor, caretDownMinor, caretUpMinor, chevronDownMinor, chevronLeftMinor, chevronRightMinor, chevronUpMinor, circleCancelMinor, circleChevronDownMinor, circleChevronLeftMinor, circleChevronRightMinor, circleChevronUpMinor, circleDisableMinor, circleInformationMajorTwotone, circlePlusMinor, circlePlusOutlineMinor, conversationMinor, deleteMinor, disputeMinor, duplicateMinor, embedMinor, exportMinor, externalMinor, homeMajorMonotone, horizontalDotsMinor, importMinor, logOutMinor, minusMinor, mobileCancelMajorMonotone, mobileHamburgerMajorMonotone, noteMinor, notificationMajorMonotone, onlineStoreMajorTwotone, ordersMajorTwotone, plusMinor, printMinor, productsMajorTwotone, profileMinor, questionMarkMajorTwotone, refreshMinor, riskMinor, saveMinor, searchMinor, tickSmallMinor, viewMinor)

type State = Unit

defaultInitialState :: State
defaultInitialState = unit

data Action
  = NoOp

type Query = Const Void

type Input = Unit

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const defaultInitialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }

render :: forall m. MonadAff m => ComponentRender m
render _ =
  HH.div_ $ (\x -> iconSvg { backdrop: false, color: Just Color.Purple} [] [x]) <$>
  [ plusMinor
  , alertMinor
  , arrowDownMinor
  , arrowLeftMinor
  , arrowRightMinor
  , arrowUpMinor
  , arrowUpDownMinor
  , calendarMinor
  , mobileCancelMajorMonotone
  , cancelSmallMinor
  , caretDownMinor
  , caretUpMinor
  , tickSmallMinor
  , chevronDownMinor
  , chevronLeftMinor
  , chevronRightMinor
  , chevronUpMinor
  , circleCancelMinor
  , circleChevronDownMinor
  , circleChevronLeftMinor
  , circleChevronRightMinor
  , circleChevronUpMinor
  , circleInformationMajorTwotone
  , circlePlusMinor
  , circlePlusOutlineMinor
  , conversationMinor
  , deleteMinor
  , circleDisableMinor
  , disputeMinor
  , duplicateMinor
  , embedMinor
  , exportMinor
  , externalMinor
  , questionMarkMajorTwotone
  , homeMajorMonotone
  , horizontalDotsMinor
  , importMinor
  , logOutMinor
  , mobileHamburgerMajorMonotone
  , noteMinor
  , notificationMajorMonotone
  , onlineStoreMajorTwotone
  , ordersMajorTwotone
  , printMinor
  , productsMajorTwotone
  , profileMinor
  , refreshMinor
  , riskMinor
  , saveMinor
  , searchMinor
  , minusMinor
  , viewMinor
  ]