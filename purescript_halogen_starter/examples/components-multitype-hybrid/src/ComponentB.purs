module ComponentB where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Int

data Query next
  = Increment next
  | GetCount (Int -> next)

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
    initialState = 0

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [ HH.p_
            [ HH.text "Current value:"
            , HH.strong_ [ HH.text (show state) ]
            ]
        , HH.button
          [ HE.onClick (HE.input_ Increment) ]
            [ HH.text ("Increment") ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Output m
    eval (Increment next) = do
      H.modify_ (_ + 1)
      pure next
    eval (GetCount reply) = do
      count <- H.get
      pure $ reply count
