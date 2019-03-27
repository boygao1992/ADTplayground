module Component.Button where

import Prelude

import Effect (Effect)

import Data.Const (Const)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = Boolean

data Query next -- Input Events, Functor Algebra for Free Monad
  = Toggle next
  | IsOn (Boolean -> next)

data Message = Toggled Boolean -- Output Events

myButton :: forall m. H.Component HH.HTML Query Unit Message m
myButton =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
  where
    initialState :: State
    initialState = false

    render :: State -> H.ComponentHTML Query
    render state =
      let
        label = if state then "On" else "Off" -- translator from data model to view model
      in
        HH.button
          [ HP.title label
          , HE.onClick (HE.input_ Toggle)
          ]
          [ HH.text label ]

                                         -- (slots)     (output message from child)
    -- type ComponentDSL s f = HalogenM s f (Const Void) Void
    -- ComponentHTML for components without children
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval (Toggle next) = do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    eval (IsOn reply) = do
      state <- H.get
      pure (reply state)
