module Polaris.UI.Block.TextStyle where

import Prelude

import DOM.HTML.Indexed (Interactive)
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

_textStylePositive = "Polaris-TextStyle--variationPositive" :: String
_textStyleNegative = "Polaris-TextStyle--variationNegative" :: String
_textStyleSubdued = "Polaris-TextStyle--variationSubdued" :: String
_textStyleStrong = "Polaris-TextStyle--variationStrong" :: String
_textStyleCode = "Polaris-TextStyle--variationCode" :: String

data VariationValue
  = Code
  | Positive
  | Negative
  | Strong
  | Subdued
derive instance genericVariationValue :: Generic VariationValue _
derive instance eqVariationValue :: Eq VariationValue
derive instance ordVariationValue :: Ord VariationValue
instance showVariationValue :: Show VariationValue where
  show = genericShow

textStyle
  :: forall p i
  . VariationValue
  -> Array (IProp (Interactive ()) i)
  -> Array (HTML p i)
  -> HTML p i
textStyle variation =
  blockBuilder
    ( if variation == Code
      then HH.code
      else HH.span
    )
    ( ClassName <$>
      [ case variation of
          Code -> _textStyleCode
          Positive -> _textStylePositive
          Negative -> _textStyleNegative
          Strong -> _textStyleStrong
          Subdued -> _textStyleSubdued
      ]
    )
