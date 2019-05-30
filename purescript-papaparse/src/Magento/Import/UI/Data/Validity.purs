module Magento.Import.UI.Data.Validity where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))


data Validity
  = Invalid
  | Validating
  | NotValidated
  | Valid
derive instance genericValidity :: Generic Validity _
derive instance eqValidity :: Eq Validity
derive instance ordValidity :: Ord Validity
instance showValidity :: Show Validity where
  show = genericShow

instance semigroupValidity :: Semigroup Validity where
  append Invalid _ = Invalid
  append _ Invalid = Invalid
  append Validating _ = Validating
  append _ Validating = Validating
  append NotValidated _ = NotValidated
  append _ NotValidated = NotValidated
  append _ _ = Valid
instance monoidValidity :: Monoid Validity where
  mempty = Invalid

borderColor :: Maybe Validity -> String
borderColor (Just v) = case v of
  NotValidated -> "border-grey"
  Validating -> "border-yellow"
  Invalid -> "border-red"
  Valid -> "border-green"
borderColor Nothing = "border-grey"

borderColorLighterer :: Maybe Validity -> String
borderColorLighterer (Just v) = case v of
  NotValidated -> "border-grey-lighter"
  Validating -> "border-yellow-lighter"
  Invalid -> "border-red-lighter"
  Valid -> "border-green-lighter"
borderColorLighterer Nothing = "border-grey-lighter"

borderColorLightestest :: Maybe Validity -> String
borderColorLightestest (Just v) = case v of
  NotValidated -> "border-grey-lightest"
  Validating -> "border-yellow-lightest"
  Invalid -> "border-red-lightest"
  Valid -> "border-green-lightest"
borderColorLightestest Nothing = "border-grey-lightest"

backgroundColor :: Maybe Validity -> String
backgroundColor (Just v)= case v of
  NotValidated -> "bg-white"
  Validating -> "bg-yellow"
  Invalid -> "bg-red"
  Valid -> "bg-green"
backgroundColor Nothing = "bg-grey"

backgroundColorLighter :: Maybe Validity -> String
backgroundColorLighter (Just v)= case v of
  NotValidated -> "bg-white-lighter"
  Validating -> "bg-yellow-lighter"
  Invalid -> "bg-red-lighter"
  Valid -> "bg-green-lighter"
backgroundColorLighter Nothing = "bg-grey-lighter"

backgroundColorLightest :: Maybe Validity -> String
backgroundColorLightest (Just v)= case v of
  NotValidated -> "bg-white-lightest"
  Validating -> "bg-yellow-lightest"
  Invalid -> "bg-red-lightest"
  Valid -> "bg-green-lightest"
backgroundColorLightest Nothing = "bg-grey-lightest"
