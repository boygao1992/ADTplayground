module Example.App.Home where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Example.App.UI.Element as UI
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-----
-- Render

render :: forall i p. HH.HTML i p
render =
  UI.section_
    [ UI.h1_ [ HH.text "Formless" ]
    , UI.h2_ [ HH.text "A renderless component for painless forms in Halogen" ]
    , UI.content_
        [ UI.p_
            """
            Formless allows you to write a small, simple spec for your form and receive state updates, validation, dirty states, submission handling, and more for free. You are responsible for providing an initial value and a validation function for every field in your form, but beyond that, Formless will take care of things behind the scenes without ever imposing on how you'd like to render and display your form. You can freely use external Halogen components, add new form behaviors on top (like dependent validation or clearing sets of fields), and more.
            """
        , UI.a
            [ HP.href "https://github.com/thomashoneyman/purescript-halogen-formless" ]
            [ HH.text "purescript-halogen-formless" ]
      ]
  ]

-- Component

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval H.defaultEval
  }

