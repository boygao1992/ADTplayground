module Formless.Types.Component where

import Prelude

import Data.Const (Const)
import Data.Functor.Variant (VariantF, FProxy)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Effect.Aff (Fiber, Milliseconds)
import Effect.Aff.AVar (AVar)
import Effect.Ref (Ref)
import Formless.Types.Form (FormFields, FormInputFields, FormInputFunction, FormInputFunctions, FormOutputFields, FormValidationAction, OutputField)
import Formless.Validation (FormValidators)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.ChildQuery (ChildQueryBox)
import Type.Row (type (+))

import Data.Lens (Lens')
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)

-- | A type representing the various functions that can be provided to extend
-- | the Formless component. Usually only the `render` function is required,
-- | but you may also provide others. For example, if you have child components,
-- | you can tell Formless how to manage those child components by adding a
-- | handler action and `handleAction` case.
type Spec form st query act slots input msg m =
  { render :: PublicState form st -> ComponentHTML form act slots m
  , handleAction :: act -> HalogenM form st act slots msg m Unit
  , handleQuery :: forall a. query a -> HalogenM form st act slots msg m (Maybe a)
  , handleEvent :: Event form st -> HalogenM form st act slots msg m Unit
  , receive :: input -> Maybe act
  , initialize :: Maybe act
  , finalize :: Maybe act
  }

-- | A simplified type when the component has only a form spec, some output, and runs
-- | in some monad `m`
type Spec' form msg input m = Spec form () (Const Void) Void () input msg m

-- | The component action type. While actions are typically considered
-- | internal to a component, in Formless you write the render function and will
-- | need to be able to use these directly. Many of these are shared with queries
-- | of the same name so they can be used either as queries or as actions. See
-- | `Formless.Action` and `Formless.Query`.
-- |
-- | You can freely extend this type with your own actions using `injAction`.
type Action form act = Variant
  ( userAction :: act
  | InternalAction act
  + PublicAction form
  )

type PublicAction form =
  ( modify :: FormInputFunction form
  , validate :: FormValidationAction form
  -- TODO , modifyValidate :: FormInputFunction form
  , modifyValidate :: Tuple (Maybe Milliseconds) (FormInputFunction form)
  -- TODO modifyValidateAsync :: Tuple Milliseconds (FormInputFunction form)
  , reset :: FormInputFunction form
  , setAll :: Tuple (FormInputFields form) Boolean
  , modifyAll :: Tuple (FormInputFunctions form) Boolean
  , resetAll :: Unit
  , validateAll :: Unit
  , submit :: Unit
  , loadForm :: FormInputFields form
  )

type InternalAction act r =
  ( initialize :: Maybe act
  , syncFormData :: Unit
  | r
  )

-- | A simple action type when the component does not need extension
type Action' form = Action form Void

-- | The internals of the public component query type. Many of these are shared
-- | with actions of the same name so they can be used in rendering. See
-- | `Formless.Action` and `Formless.Query` for more.
data QueryF form slots a
  = SubmitReply (Maybe (form Record OutputField) -> a)
  -- Query a child component of Formless through Formless
  | SendQuery (ChildQueryBox slots (Maybe a))
  -- Run a Formless action as a query
  | AsQuery (Variant (PublicAction form)) a

derive instance functorQueryF :: Functor (QueryF form slots)

-- | The component query type, which you can freely extend with your own queries
-- | using `injQuery` from `Formless.Query`.
type Query form query slots = VariantF
  ( query :: FProxy (QueryF form slots)
  , userQuery :: FProxy query
  )

-- | A simple query type when the component does not need extension
type Query' form = Query form (Const Void) ()

-- | The component type
type Component form query slots input msg m =
  H.Component HH.HTML (Query form query slots) input msg m

-- | A simple component type when the component does not need extension
type Component' form input m =
  Component form (Const Void) () input Void m

-- | The component's HTML type, the result of the render function.
type ComponentHTML form act slots m =
  H.ComponentHTML (Action form act) slots m

-- | A simple component HTML type when the component does not need extension
type ComponentHTML' form m =
  ComponentHTML form Void () m

-- | The component's eval type
type HalogenM form st act slots msg m =
  H.HalogenM (State form st m) (Action form act) slots msg m

-- | A simple component eval type when the component does not need extension
type HalogenM' form msg m =
  HalogenM form () Void () msg m

-- | The component local state
type State form st m =
  -- { | PublicStateRow form (internal :: InternalState form m | st) }
  { internal :: InternalState form m
  | PublicStateRow form
  + st
  }

_internal :: forall form st m. Lens' (State form st m) (InternalState form m)
_internal = prop (SProxy :: SProxy "internal")
_InternalState :: forall form st m. Lens' (State form st m) (Record (InternalStateRow form m))
_InternalState = _internal <<< _Newtype

-- | A simple state type when the component does not need extension
type State' form m =
  State form () m

-- | The component's public state
type PublicState form st =
  { | PublicStateRow form st }

-- | The component's public state, as an extensible row
type PublicStateRow form st =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: FormFields form
  | st
  )

_form :: forall form st m. Lens' (State form st m) (FormFields form)
_form = prop (SProxy :: SProxy "form")

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
type InternalStateRow form m =
  ( initialInputs :: FormInputFields form
  , validators :: FormValidators form m
  , allTouched :: Boolean
  , debounceRef :: Maybe (Ref (Maybe Debouncer)) -- TODO deprecate
  , debouncerFields :: Map String DebouncerField -- TODO
  , validationRef :: Maybe (Ref (Maybe H.ForkId)) -- NOTE new
  )

_validators :: forall form st m. Lens' (State form st m) (FormValidators form m)
_validators = _InternalState <<< prop (SProxy :: SProxy "validators")

_debouncerFields
  :: forall form st m
  . Lens' (State form st m) (Map String DebouncerField)
_debouncerFields = _InternalState <<< prop (SProxy :: SProxy "debouncerFields")

_debouncerFieldM
  :: forall form st m
  . String -> Lens' (State form st m) (Maybe DebouncerField)
_debouncerFieldM label = _debouncerFields <<< at label

newtype InternalState form m = InternalState { | InternalStateRow form m }

derive instance newtypeInternalState :: Newtype (InternalState form m) _

type DebouncerField = Record DebouncerFieldRow

type DebouncerFieldRow =
  ( debouncer :: Ref (Maybe Debouncer)
  , canceller :: Ref (Maybe H.ForkId)
  )

-- | A type to represent a running debouncer
type Debouncer =
  { channel :: AVar Unit
  , fiber :: Fiber Unit
  }

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

-- | The component's input type. If you provide `Nothing` as your `initialInputs`
-- | then the form will fill in values based on the `Initial` type class for the
-- | field's input type. Otherwise, the form will contain the values you provide.
-- |
-- | Validators can be created using the Formless.Validation module.
type Input form st m =
  { initialInputs :: Maybe (FormInputFields form)
  , validators :: FormValidators form m
  | st
  }

-- | A simple Input type when the component does not need extension
type Input' form m = Input form () m

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end, or extend these messages.
data Event form st
  = Submitted (FormOutputFields form)
  | Changed (PublicState form st)

type Event' form = Event form ()

-- | A slot type that can be used in the ChildSlots definition for your parent
-- | component
type Slot form query slots msg = H.Slot (Query form query slots) msg

-- | A simple Slot type when the component does not need extension, besides a
-- | custom output message
type Slot' form msg = H.Slot (Query' form) msg

-- | A convenience export of formless as a symbol for use when mounting Formless
-- | as a child component
-- |
-- | ```purescript
-- | type ChildSlots = (formless :: F.Slot' Form FormResult)
-- | HH.slot F._formless unit (F.component spec) input handler
-- | ```
_formless = SProxy :: SProxy "formless"
