module Formless.Type.Lenses where

import Prelude

import Data.Lens (Lens', Traversal', _Just)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Ref (Ref)
import Formless.Types.Component (Canceller, Debouncer, DebouncerField, InternalState, InternalStateRow, State)
import Type.Data.Symbol (SProxy(..))


_internal :: forall form m. Lens' (State form m) (InternalState form m)
_internal = prop (SProxy :: SProxy "internal")

_InternalState :: forall form m. Lens' (State form m) (Record (InternalStateRow form m))
_InternalState = _internal <<< _Newtype

_debouncerFields :: forall form m. Lens' (State form m) (Map String (DebouncerField m))
_debouncerFields = _InternalState <<< prop (SProxy :: SProxy "debouncerFields")

_debouncerField :: forall form m. String -> Lens' (State form m) (Maybe (DebouncerField m))
_debouncerField label = _debouncerFields <<< at label

_debouncerM :: forall form m. String -> Traversal' (State form m) (Maybe (Ref (Maybe Debouncer)))
_debouncerM label = _debouncerField label <<< _Just <<< prop (SProxy :: SProxy "debouncer")

_debouncerRef :: forall form m. String -> Traversal' (State form m) (Ref (Maybe Debouncer))
_debouncerRef label = _debouncerM label <<< _Just

_cancellerM :: forall form m. String -> Traversal' (State form m) (Maybe (Ref (Maybe (Canceller m))))
_cancellerM label = _debouncerField label <<< _Just <<< prop (SProxy :: SProxy "canceller")

_cancellerRef :: forall form m. String -> Traversal' (State form m) (Ref (Maybe (Canceller m)))
_cancellerRef label = _cancellerM label <<< _Just


{-
type State form m =
  { validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: FormFields form
  , internal :: InternalState form m
  }

newtype InternalState form m = InternalState
  { initialInputs :: FormInputFields form
  , validators :: FormValidators form m
  , allTouched :: Boolean
  , debouncerFields :: Map.Map String (DebouncerField m)
  }

type DebouncerField m =
  { debouncer :: Maybe (Ref (Maybe Debouncer))
  , canceller :: Maybe (Ref (Maybe (Canceller m)))
  }

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

type Canceller m = Error -> m Unit

-}

