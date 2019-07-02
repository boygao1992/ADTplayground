module Polaris.UI.Block.Stack where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Monoid (guard)
import Halogen.HTML (ClassName(..), HTML, IProp)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_stack = "Polaris-Stack" :: String
_stack_noWrap = "Polaris-Stack--noWrap" :: String
_stack_vertical = "Polaris-Stack--vertical" :: String

_stackItem = "Polaris-Stack__Item" :: String
_stackItem_fill = "Polaris-Stack__Item--fill" :: String

data Spacing
  = SpacingNone
  | SpacingExtraTight
  | SpacingTight
  | SpacingNormal
  | SpacingLoose
  | SpacingExtraLoose
_stack_spacingNone = "Polaris-Stack--spacingNone" :: String
_stack_spacingExtraTight = "Polaris-Stack--spacingExtraTight" :: String
_stack_spacingTight = "Polaris-Stack--spacingTight" :: String
_stack_spacingLoose = "Polaris-Stack--spacingLoose" :: String
_stack_spacingExtraLoose = "Polaris-Stack--spacingExtraLoose" :: String


data Distribution
  = DistributionNone
  | DistributionLeading
  | DistributionCenter
  | DistributionTrailing
  | DistributionEqualSpacing
  | DistributionFill
  | DistributionFillEvenly
_stack_distributionLeading = "Polaris-Stack--distributionLeading" :: String
_stack_distributionCenter = "Polaris-Stack--distributionCenter" :: String
_stack_distributionTrailing = "Polaris-Stack--distributionTrailing" :: String
_stack_distributionEqualSpacing = "Polaris-Stack--distributionEqualSpacing" :: String
_stack_distributionFill = "Polaris-Stack--distributionFill" :: String
_stack_distributionFillEvenly = "Polaris-Stack--distributionFillEvenly" :: String


data Alignment
  = AlignmentStretch
  | AlignmentLeading
  | AlignmentCenter
  | AlignmentTrailing
  | AlignmentFill
  | AlignmentBaseline
_stack_alignmentLeading = "Polaris-Stack--alignmentLeading" :: String
_stack_alignmentCenter = "Polaris-Stack--alignmentCenter" :: String
_stack_alignmentTrailing = "Polaris-Stack--alignmentTrailing" :: String
_stack_alignmentFill = "Polaris-Stack--alignmentFill" :: String
_stack_alignmentBaseline = "Polaris-Stack--alignmentBaseline" :: String


type StackOptions =
  { vertical :: Boolean
  , spacing :: Spacing
  , distribution :: Distribution
  , alignment :: Alignment
  , noWrap :: Boolean
  }

defaultOptions :: StackOptions
defaultOptions =
  { vertical: false
  , spacing: SpacingNormal
  , distribution: DistributionNone
  , alignment: AlignmentStretch
  , noWrap: false
  }

stack
  :: forall p i
  . StackOptions
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
stack { spacing, distribution, alignment, vertical, noWrap }
  = blockBuilder HH.div $ ClassName <$>
    [ _stack
    , case spacing of
        SpacingNone -> _stack_spacingNone
        SpacingExtraTight -> _stack_spacingExtraTight
        SpacingTight -> _stack_spacingTight
        SpacingNormal -> mempty
        SpacingLoose -> _stack_spacingLoose
        SpacingExtraLoose -> _stack_spacingExtraLoose
    , case distribution of
        DistributionNone -> mempty
        DistributionLeading -> _stack_distributionLeading
        DistributionCenter -> _stack_distributionCenter
        DistributionTrailing -> _stack_distributionTrailing
        DistributionEqualSpacing -> _stack_distributionEqualSpacing
        DistributionFill -> _stack_distributionFill
        DistributionFillEvenly -> _stack_distributionFillEvenly
    , case alignment of
        AlignmentStretch -> mempty
        AlignmentLeading -> _stack_alignmentLeading
        AlignmentCenter -> _stack_alignmentCenter
        AlignmentTrailing -> _stack_alignmentTrailing
        AlignmentFill -> _stack_alignmentFill
        AlignmentBaseline -> _stack_alignmentBaseline
    , _stack_vertical # guard vertical
    , _stack_noWrap # guard noWrap
    ]


stack_
  :: forall p i
  . StackOptions
  -> Array (HTML p i)
  -> HTML p i
stack_ config = stack config []

stackItem
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
stackItem = blockBuilder HH.div [ ClassName _stackItem ]

stackItem_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
stackItem_ = stackItem []

stackItem_fill
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
stackItem_fill = blockBuilder HH.div [ ClassName _stackItem_fill ]

stackItem_fill_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
stackItem_fill_ = stackItem_fill []
