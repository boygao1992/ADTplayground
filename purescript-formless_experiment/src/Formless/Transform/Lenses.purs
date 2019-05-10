module Formless.Type.Lenses where

import Prelude

import Data.Lens (Lens')
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Formless.Types.Component (DebouncerField, InternalState, InternalStateRow, State)
import Formless.Validation (FormValidators, FormFields)
import Type.Data.Symbol (SProxy(..))


_internal :: forall form m. Lens' (State form m) (InternalState form m)
_internal = prop (SProxy :: SProxy "internal")

_form :: forall form m. Lens' (State form m) (FormFields form)
_form = prop (SProxy :: SProxy "form")

_InternalState :: forall form m. Lens' (State form m) (Record (InternalStateRow form m))
_InternalState = _internal <<< _Newtype

_debouncerFields :: forall form m. Lens' (State form m) (Map String (DebouncerField m))
_debouncerFields = _InternalState <<< prop (SProxy :: SProxy "debouncerFields")

_validators :: forall form m. Lens' (State form m) (FormValidators form m)
_validators = _InternalState <<< prop (SProxy :: SProxy "validators")

_debouncerFieldM
  :: forall form m. String -> Lens' (State form m) (Maybe (DebouncerField m))
_debouncerFieldM label = _debouncerFields <<< at label

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
  { debouncer :: Ref (Maybe Debouncer)
  , canceller :: Ref (Maybe (Canceller m))
  }

type Debouncer =
  { channel :: AVar Unit
  , fiber :: Fiber Unit
  }

type Canceller m = Error -> m Unit

-}

