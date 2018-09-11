module Task where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { description :: String
  , completed :: Boolean
  }

data Query next
  = UpdateDescription String next
  | ToggleCompleted Boolean next
  | Remove next
  | IsCompleted (Boolean -> next)

type Input = Unit

data Output
  = Removed
  | Toggled Boolean

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
    initialState =
      { description : ""
      , completed : false
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.li_
        [ HH.input [ HP.type_ HP.InputCheckbox
                   , HP.title "Mark as completed"
                   , HP.checked state.completed
                   , HE.onChecked (HE.input ToggleCompleted)
                   ]
        , HH.input [ HP.type_ HP.InputText
                   , HP.placeholder "Task description"
                   , HP.autofocus true
                   , HP.value state.description
                   , HE.onValueChange (HE.input UpdateDescription)
                   ]
        , HH.button [ HP.title "Remove task"
                    , HE.onClick (HE.input_ Remove)
                    ]
            [ HH.text "x" ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Output m
    eval (UpdateDescription desc next) = do
      H.modify_ (_ { description = desc })
      pure next
    eval (ToggleCompleted b next) = do
      H.modify_ (_ { completed = b })
      H.raise (Toggled b)
      pure next
    eval (Remove next) = do
      H.raise Removed
      pure next
    eval (IsCompleted reply) = do
      reply <$> H.gets (_.completed)
