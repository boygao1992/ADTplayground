module Formless.Types.Component where

import Prelude

import Control.Comonad.Store (Store)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Error, Fiber, Milliseconds)
import Effect.Aff.AVar (AVar)
import Effect.Ref (Ref)
import Formless.Validation (FormFields, FormInputFields, FormInputFunction, FormInputFunctions, FormOutputFields, FormValidationAction, FormValidators)
import Halogen as H
import Halogen.HTML as HH


-- | The component query type. See Formless.Query for helpers related
-- | to constructing and using these queries.
data Query parent_query child_query child_slots form m a
  = Modify (FormInputFunction form) a
  | Validate (FormValidationAction form) a
  | ModifyValidate (FormInputFunction form) a
  | ModifyValidateAsync Milliseconds (FormInputFunction form) a
  | Reset (FormInputFunction form) a
  | SetAll (FormInputFields form) a
  | ModifyAll (FormInputFunctions form) a
  | ResetAll a
  | ValidateAll a
  | Submit a
  | SubmitReply (Maybe (FormOutputFields form) -> a)
  | GetState (PublicState form -> a)
  | Send child_slots (child_query a)
  | LoadForm (FormInputFields form) a
  | SyncFormData a
  | Raise (parent_query Unit) a
  | Initialize a
  | Receive (Input parent_query child_query child_slots form m) a
  | AndThen
      (Query parent_query child_query child_slots form m Unit)
      (Query parent_query child_query child_slots form m Unit)
      a

-- | The overall component state type, which contains the local state type
-- | and also the render function
type StateStore parent_query child_query child_slots form m
  = Store (State form m) (HTML parent_query child_query child_slots form m)

-- | The component type
type Component parent_query child_query child_slots form m
  = H.Component
      HH.HTML
      (Query parent_query child_query child_slots form m)
      (Input parent_query child_query child_slots form m)
      (Message parent_query form)
      m

-- | The component's HTML type, the result of the render function.
type HTML parent_query child_query child_slots form m
  = H.ParentHTML
    (Query parent_query child_query child_slots form m) child_query child_slots m

-- | The component's DSL type, the result of the eval function.
type DSL parent_query child_query child_slots form m
  = H.ParentDSL
      (StateStore parent_query child_query child_slots form m)
      (Query parent_query child_query child_slots form m)
      child_query
      child_slots
      (Message parent_query form)
      m

-- | The component local state
type State form m = Record (StateRow form m)

type StateRow form m = PublicStateRow form (internal :: InternalState form m)

-- | The component's public state
type PublicState form = Record (PublicStateRow form ())

-- | The component's public state
type PublicStateRow form r =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: FormFields form
  | r
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form m = InternalState (Record (InternalStateRow form m))
derive instance newtypeInternalState :: Newtype (InternalState form m) _

type InternalStateRow form m =
  ( initialInputs :: FormInputFields form
  , validators :: FormValidators form m
  , allTouched :: Boolean
  , debouncerFields :: Map.Map String (DebouncerField m)
  )

type DebouncerField m = Record (DebouncerFieldRow m)

type DebouncerFieldRow m =
  ( debouncer :: Maybe (Ref (Maybe Debouncer))
  , canceller :: Maybe (Ref (Maybe (Canceller m)))
  )

type Debouncer = Record DebouncerRow

type DebouncerRow =
  ( var :: AVar Unit
  , fiber :: Fiber Unit
  )

type Canceller m = Error -> m Unit

-- | A type to represent validation status
data ValidStatus
  = Invalid
  | Incomplete
  | Valid

derive instance genericValidStatus :: Generic ValidStatus _
derive instance eqValidStatus :: Eq ValidStatus
derive instance ordValidStatus :: Ord ValidStatus

instance showValidStatus :: Show ValidStatus where
  show = genericShow

-- | The component's input type
type Input parent_query child_query child_slots form m =
  { initialInputs :: FormInputFields form
  , validators :: FormValidators form m
  , render :: State form m -> HTML parent_query child_query child_slots form m
  }

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end.
data Message parent_query form
  = Submitted (FormOutputFields form)
  | Changed (PublicState form)
  | Emit (parent_query Unit)

-- | Simple types

-- | A simple query type when you have no child slots in use
type Query' form m = Query (Const Void) (Const Void) Void form m

-- | A simple HTML type when the component does not need embedding
type HTML' form m = H.ParentHTML (Query' form m) (Const Void) Void m

-- | A simple Message type when the component does not need embedding
type Message' form = Message (Const Void) form

-- | A simple Input type when the component does not need embedding
type Input' form m = Input (Const Void) (Const Void) Void form m
