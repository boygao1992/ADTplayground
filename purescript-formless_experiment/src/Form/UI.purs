module Form.UI where

import Prelude

import DOM.HTML.Indexed (HTMLa, HTMLbutton, HTMLinput, HTMLtextarea)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Form.Validation (class ToText, showError)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Data.Symbol (SProxy)

-- | Types

type Plain i p = Array (HH.HTML i p) -> HH.HTML i p

type FieldConfig' =
  { label :: String
  , help :: Either String String
  , placeholder :: String
  }

type FieldConfig sym =
  { label :: String
  , help :: String
  , placeholder :: String
  , sym :: SProxy sym
  }

-- | Utils

css :: ∀ r t. String -> HH.IProp ( "class" :: String | r ) t
css = HP.class_ <<< HH.ClassName

-- | Layout

section_ :: ∀ i p. Plain i p
section_ content =
  HH.section
  [ css "section columns" ]
  [ HH.div
    [ css "column is-11-tablet is-three-fifths-desktop" ]
    content
  ]

formContent_ :: ∀ i p. Plain i p
formContent_ content =
  HH.div
  [ css "content" ]
  [ HH.div
    [ css "column has-background-white-bis" ]
    content
  ]

content_ :: ∀ i p. Plain i p
content_ = HH.div [ css "content" ]

grouped_ :: ∀ i p. Plain i p
grouped_ array =
  HH.div
  [ css "field is-grouped" ]
  ( wrap <$> array )

  where
    wrap x = HH.p [ css "control" ] [ x ]

-- | Typography

h1_ :: ∀ i p. Plain i p
h1_ = HH.h1 [ css "title" ]

h2_ :: ∀ i p. Plain i p
h2_ = HH.h2 [ css "subtitle is-size-4 has-text-grey" ]

p_ :: ∀ i p. String -> HH.HTML i p
p_ str = HH.p_ [ HH.text str ]

a :: ∀ i p. Array (HH.IProp HTMLa p) -> Plain i p
a props = HH.a ([ css "has-text-blue" ] <> props)

-- | Button

buttonPrimary :: ∀ i p. Array (HH.IProp HTMLbutton p) -> Plain i p
buttonPrimary props = HH.button ([ css "button is-link" ] <> props)

button :: ∀ i p. Array (HH.IProp HTMLbutton p) -> Plain i p
button props = HH.button ([ css "button is-light" ] <> props)

buttonDark :: ∀ i p. Array (HH.IProp HTMLbutton p) -> Plain i p
buttonDark props = HH.button ([ css "button is-dark" ] <> props)

-- | Form

field :: ∀ i p. { label :: String, help :: Either String String } -> Plain i p
field config contents =
  HH.div
  [ css "field" ]
  [ HH.div
    [ css "label" ]
    [ HH.text config.label ]
  , HH.div
    [ css "control" ]
    contents
  , case config.help of
      Left str -> helpError_ str
      Right str -> help_ str
  ]
  where
    help_ str = HH.p [ css "help" ] [ HH.text str ]
    helpError_ str = HH.p [ css "help is-danger" ] [ HH.text str ]

-- | Formless

input :: ∀ i p. FieldConfig' -> Array (HH.IProp HTMLinput p) -> HH.HTML i p
input config props =
  field
    { label: config.label, help: config.help }
    [ HH.input
      ( [ HP.type_ InputText
        , either (const $ css "input is-danger") (const $ css "input") config.help
        , HP.placeholder config.placeholder
        ] <> props
      )
    ]

textarea :: ∀ i p. FieldConfig' -> Array (HH.IProp HTMLtextarea p) -> HH.HTML i p
textarea config props =
  field
    { label: config.label, help: config.help }
    [ HH.textarea
      ( [ either (const $ css "textarea is-danger") (const $ css "textarea") config.help
        , HP.placeholder config.placeholder
        ] <> props
      )
    ]

resultToHelp
  :: ∀ t e
  . ToText e
  => String
  -> F.FormFieldResult e t
  -> Either String String
resultToHelp str = case _ of
  F.NotValidated -> Right str
  F.Validating -> Right "validating..."
  other -> maybe (Right str) Left $ showError other
