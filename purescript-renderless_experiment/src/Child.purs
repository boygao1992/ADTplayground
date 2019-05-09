module Child where

import Prelude

import Renderless.State (modifyStore_)
import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query o a
  = Raise (o Unit) a
  | Receive (Input o) a

type StateStore o =
  Store State (H.ComponentHTML (Query o))

type State = Unit

type Input o = { render :: State -> H.ComponentHTML (Query o)}

data Output o
  = Emit (o Unit)

initialState :: forall o. Input o -> StateStore o
initialState { render } = store render unit

eval
  :: forall o m
  . Query o
  ~> H.ComponentDSL (StateStore o) (Query o) (Output o) m
eval = case _ of
  Raise query a -> a <$ do
    H.raise (Emit query)

  Receive { render } a -> a <$ do
    modifyStore_ render identity

component
  :: forall m o
  . MonadAff m
  => H.Component HH.HTML (Query o) (Input o) (Output o) m
component = H.component
  { initialState
  , render: extract
  , eval
  , receiver: HE.input Receive
  }
