module ComponentC where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = String

data Query next
  = HandleInput String next
  | GetValue (String -> next)

type Input = Unit

type Output = Void

component :: forall m. H.Component HH.HTML Query Input Output m
component =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
  where
    initialState :: State
    initialState = "Hi"

    render :: State -> H.ComponentHTML Query
    render state =
      HH.label_
        [ HH.p_ [ HH.text "What do you have to say?" ]
        , HH.input
          [ HP.value state
          , HE.onValueInput (HE.input HandleInput)
          ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Output m
    eval (HandleInput value next) = do
      H.put value
      pure next
    eval (GetValue reply) = do
      state <- H.get
      pure $ reply state

