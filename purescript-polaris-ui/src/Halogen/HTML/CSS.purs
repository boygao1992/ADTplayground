module Halogen.HTML.CSS
  ( style
  , stylesheet
  ) where

import Prelude

import CSS.Property (Key, Value)
import CSS.Render (render, renderedSheet, collect)
import CSS.Stylesheet (CSS, Rule(..), runS)

import Data.Array (mapMaybe, concatMap, singleton)
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

import Halogen.HTML as HH
import Halogen.HTML.Elements as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC

-- | Render a set of rules as an inline style.
-- |
-- | For example:
-- |
-- | ```purescript
-- | HH.div [ CSS.style do color red
-- |                      display block ]
-- |       [ ... ]
-- | ```
style :: forall i r. CSS -> HP.IProp (style :: String | r) i
style =
  HP.attr (HC.AttrName "style")
    <<< toString
    <<< rules
    <<< runS
  where
  toString :: Object.Object String -> String
  toString = joinWith "; " <<< Object.foldMap (\key val -> [ key <> ": " <> val])

  rules :: Array Rule -> Object.Object String
  rules rs = Object.fromFoldable properties
    where
    properties :: Array (Tuple String String)
    properties = mapMaybe property rs >>= collect >>> rights

  property :: Rule -> Maybe (Tuple (Key Unit) Value)
  property (Property k v) = Just (Tuple k v)
  property _              = Nothing

  rights :: forall a b. Array (Either a b) -> Array b
  rights = concatMap $ foldMap singleton

-- | Render a set of rules as a `style` element.
stylesheet :: forall p i. CSS -> HC.HTML p i
stylesheet css =
  HE.style [ HP.type_ $ MediaType "text/css" ] [ HH.text content ]
  where
    content = fromMaybe "" $ renderedSheet $ render css
