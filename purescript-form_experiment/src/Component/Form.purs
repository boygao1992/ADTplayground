module Component.Form where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.Form as Form

data Query next
type Input = Unit
type Output = Void
-- type IO = Aff

type Component m = H.Component HH.HTML Query Unit Void m

newtype FormSlot = FormSlot Int
derive instance eqFormSlot :: Eq FormSlot
derive instance ordFormSlot :: Ord FormSlot


-- form :: forall m. Component m
