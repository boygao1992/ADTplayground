module Examples.EffectsEffRandom.Main where

import Prelude

import Control.Coroutine as CR
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Random (random)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = Maybe Number

initialState :: State
initialState = Nothing

data Query a
  = Regenerate a
  | UpdateNumber Number a

type InMsg = Unit
data OutMsg = RandomNew

render :: State -> H.ComponentHTML Query
render state =
  let
    value = maybe "No number generated yet" show state
  in
    HH.div_
      [ HH.h1_ [ HH.text "Random number"]
      , HH.p_ [ HH.text $ "Current value: " <> value ]
      , HH.button
        [ HE.onClick (HE.input_ Regenerate) ]
        [ HH.text "Generate new nubmer" ]
      ]

eval :: forall m. Query ~> H.ComponentDSL State Query OutMsg m
eval (Regenerate next) = do
  H.raise RandomNew
  pure next
eval (UpdateNumber newNumber next) = do
  H.put $ Just newNumber
  pure next

ui :: H.Component HH.HTML Query InMsg OutMsg Aff
ui =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI ui unit body

  io.subscribe $ CR.consumer \(RandomNew) -> do
    newNumber <- H.liftEffect random
    H.liftEffect $ log $ "New number is generated" <> show newNumber
    io.query $ H.action $ UpdateNumber newNumber
    pure Nothing
