module Parent where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Renderless.State (modifyStore_)

data Query o m a
  = Raise (o Unit) a
  | Receive (Input o m) a

type StateStore o m =
  Store State (H.ParentHTML (Query o m) ChildQuery ChildSlot m)

type State = Unit

type Input o m =
  { render :: State -> H.ParentHTML (Query o m) ChildQuery ChildSlot m }

data Output o
  = Emit (o Unit)

type ChildQuery = Const Void
type ChildSlot = Unit

initialState :: forall o m. Input o m -> StateStore o m
initialState { render } = store render unit

eval
  :: forall o m
  . Query o m
  ~> H.ParentDSL (StateStore o m) (Query o m) ChildQuery ChildSlot (Output o) m
eval = case _ of
  Raise query a -> a <$ do
    H.raise (Emit query)

  Receive { render } a -> a <$ do
    modifyStore_ render identity

component
  :: forall o m
  . MonadAff m
  => H.Component HH.HTML (Query o m) (Input o m) (Output o) m
component = H.parentComponent
  { initialState
  , render: extract
  , eval
  , receiver: HE.input Receive
  }
