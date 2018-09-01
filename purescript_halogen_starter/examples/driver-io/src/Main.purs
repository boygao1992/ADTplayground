module Examples.DriverIO.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Control.Coroutine as CR
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = Boolean

initialState :: State
initialState = false

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

type InMsg = Unit
data OutMsg = Toggled Boolean

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

eval :: forall m. Query ~> H.ComponentDSL State Query OutMsg m
eval (Toggle next) = do
  currentState <- H.get
  let nextState = not currentState
  H.put nextState
  H.raise $ Toggled nextState
  pure next
eval (IsOn reply) = do
  state <- H.get
  pure $ reply state

component_button :: forall m. H.Component HH.HTML Query InMsg OutMsg m
component_button =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI component_button unit body

  io.subscribe $ CR.consumer \(Toggled newState) -> do
    H.liftEffect $ log $ "Button was toggled to: " <> show newState
    pure Nothing

  io.query $ H.action $ Toggle
  io.query $ H.action $ Toggle
  io.query $ H.action $ Toggle
