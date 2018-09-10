module Container where

import Prelude

import ComponentA as CA
import ComponentB as CB
import ComponentC as CC
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { a :: Maybe CA.State
  , b :: Maybe CB.State
  , c :: Maybe CC.State
  }

data Query next
  = ReadStates next

type Input = Unit

type Output = Void

type ChildQuery = Coproduct3 CA.Query CB.Query CC.Query
-- data ChildQuery next
--   = QueryA (CA.Query next)
--   | QueryB (CB.Query next)
--   | QueryC (CC.Query next)

type ChildSlot = Either3 CA.Input CB.Input CC.Input

component :: forall m. H.Component HH.HTML Query Input Output m
component =
  H.parentComponent
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
  where
    initialState :: State
    initialState = { a : Nothing, b : Nothing, c : Nothing }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render state =
      HH.div_
        [ HH.div [ HP.class_ (H.ClassName "box") ]
            [ HH.h1_ [ HH.text "ComponentA" ]
            , HH.slot' CP.cp1 unit CA.component unit absurd
            ]
        , HH.div [ HP.class_ (H.ClassName "box") ]
            [ HH.h1_ [ HH.text "ComponentB" ]
            , HH.slot' CP.cp2 unit CB.component unit absurd
            ]
        , HH.div [ HP.class_ (H.ClassName "box") ]
            [ HH.h1_ [ HH.text "ComponentC" ]
            , HH.slot' CP.cp3 unit CC.component unit absurd
            ]
        , HH.p_
            [ HH.text "Last observed states:" ]
        , HH.ul_
            [ HH.li_
                [ HH.text ("ComponentA: " <> show state.a) ]
            , HH.li_
                [ HH.text ("ComponentB: " <> show state.b) ]
            , HH.li_
                [ HH.text ("ComponentC: " <> show state.c) ]
            ]
        , HH.button [ HE.onClick (HE.input_ ReadStates) ]
            [ HH.text "Check states now" ]
        ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval (ReadStates next) = do
      a <- H.query' CP.cp1 unit (H.request CA.GetState)
      b <- H.query' CP.cp2 unit (H.request CB.GetCount)
      c <- H.query' CP.cp3 unit (H.request CC.GetValue)
      H.put { a, b, c }
      pure next
