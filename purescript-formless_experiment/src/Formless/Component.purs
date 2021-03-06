module Formless.Component where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (store)
import Control.Monad.Free (liftF)
import Data.Coyoneda (liftCoyoneda)
import Data.Eq (class EqRecord)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_, for_)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Formless.Internal.Transform as Internal
import Formless.Type.Lenses (_debouncerFieldM, _form)
import Formless.Types.Component (Component, DSL, DebouncerField, Input, InternalState(..), Message(..), PublicState, Query(..), State, StateStore, ValidStatus(..), Debouncer, Canceller)
import Formless.Validation (FormFields, FormInputField, FormInputFields, FormInputFunction, FormInputFunctions, FormOutputFields, FormValidationAction, FormValidators)
import Halogen as H
import Halogen.HTML.Events as HE
import Prim.RowList as RL
import Record as Record
import Renderless.State (assignState, getState, modifyState, modifyState_, modifyStore_, modifyingState, useState)
import Unsafe.Coerce (unsafeCoerce)

-- | The Formless component
component
  :: ∀ parent_query child_query child_slots form m
      is ixs ivs fs fxs us vs os ifs ivfs
  . Ord child_slots
  => MonadAff m
  => RL.RowToList is ixs
  => RL.RowToList fs fxs
  => EqRecord ixs is
  => Internal.InputFieldsToFormFields ixs is fs
  => Internal.FormFieldsToInputFields fxs fs is
  => Internal.CountErrors fxs fs
  => Internal.AllTouched fxs fs
  => Internal.SetFormFieldsTouched fxs fs fs
  => Internal.ReplaceFormFieldInputs is fxs fs fs
  => Internal.ModifyAll ifs fxs fs fs
  => Internal.ValidateAll vs fxs fs fs m
  => Internal.FormFieldToMaybeOutput fxs fs os
  => Newtype (FormInputField form) (Variant ivs)
  => Newtype (FormInputFields form) { | is }
  => Newtype (FormOutputFields form) { | os }
  => Newtype (FormValidationAction form) (Variant us)
  => Newtype (FormInputFunction form) (Variant ivfs)
  => Newtype (FormInputFunctions form) { | ifs }
  => Newtype (FormFields form) { | fs }
  => Newtype (FormValidators form m) { | vs }
  => Component parent_query child_query child_slots form m
component =
  H.lifecycleParentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where

  initialState
    :: Input parent_query child_query child_slots form m
    -> StateStore parent_query child_query child_slots form m
  initialState { initialInputs, validators, render } = store render $
    { validity: Incomplete
    , dirty: false
    , errors: 0
    , submitAttempts: 0
    , submitting: false
    , form: Internal.inputFieldsToFormFields initialInputs
    , internal: InternalState
        { allTouched: false
        , initialInputs
        , validators
        , debouncerFields: Map.empty
        }
    }

  eval
    :: Query parent_query child_query child_slots form m
    ~> DSL parent_query child_query child_slots form m
  eval = case _ of
    Initialize a -> a <$ do
      -- NOTE debug
      H.liftEffect $ log "Initialize"

      -- dr <- H.liftEffect $ Ref.new Nothing
      -- vr <- H.liftEffect $ Ref.new Nothing
      -- modifyState_ \st -> st
      --   { internal = over InternalState
      --       (_
      --         { debounceRef = Just dr
      --         , validationRef = Just vr
      --         }
      --       )
      --       st.internal
      --   }

    Modify variant a -> do
      -- NOTE debug
      H.liftEffect $ log "Modify"

      modifyState_ \st -> st
        { form = Internal.unsafeModifyInputVariant identity variant st.form }
      eval $ SyncFormData a

    Validate variant a -> do
      st <- getState
      form <- H.lift
        $ Internal.unsafeRunValidationVariant variant (unwrap st.internal).validators st.form
      modifyState_ _ { form = form }
      eval $ SyncFormData a

    -- Provided as a separate query to minimize state updates / re-renders
    ModifyValidate variant a -> do
      -- NOTE debug
      H.liftEffect $ log "ModifyValidate"

      let
        action :: FormValidationAction form
        action = unsafeCoerce variant -- NOTE it's ok to forget value types since only its label is used to pick out the correct validator from vs

      modifyingState _form (Internal.unsafeModifyInputVariant identity variant)
      eval (Validate action a)

    ModifyValidateAsync ms variant a -> a <$ do
      -- NOTE Signal: Field_Update

      -- NOTE debug
      H.liftEffect $ log "ModifyValidateAsync"

      modifyingState _form (Internal.unsafeModifyInputVariant identity variant)

      let
        label :: String
        label = case unsafeCoerce (unwrap variant) of
          VariantRep x -> x.type

      debouncerFieldM :: Maybe (DebouncerField m)
        <- useState (_debouncerFieldM label)

      debouncerField <- case debouncerFieldM of
        Nothing -> do
          debouncer <- H.liftEffect $ Ref.new Nothing
          canceller <- H.liftEffect $ Ref.new Nothing
          let debouncerFieldRow = { debouncer, canceller }
          assignState (_debouncerFieldM label) (Just debouncerFieldRow)
          pure debouncerFieldRow
        Just df -> do
          pure df

      let
        debouncerRef :: Ref (Maybe Debouncer)
        debouncerRef = debouncerField.debouncer

        cancellerRef :: Ref (Maybe (Canceller m))
        cancellerRef = debouncerField.canceller

        scheduleDebounceSignal
          :: AVar.AVar Unit
          -> DSL parent_query child_query child_slots form m (Aff.Fiber Unit)
        scheduleDebounceSignal channel = H.liftAff $ Aff.forkAff do
          Aff.delay ms
          AVar.put unit channel

        interceptDebounceSignal
          :: Aff.Fiber Unit
          -> DSL parent_query child_query child_slots form m Unit
        interceptDebounceSignal =
          void <<< H.liftAff <<< Aff.killFiber (Aff.error "renew delayed signal")

      debouncerM <- H.liftEffect $ Ref.read debouncerRef
      cancellerM <- H.liftEffect $ Ref.read cancellerRef

      -- NOTE Field_Update -> intercept validation
      for_ cancellerM \canceller -> do
        -- NOTE debug
        H.liftEffect $ log "validation intercepted"

        H.lift $ canceller (Aff.error "intercept validation")
        H.liftEffect $ Ref.write Nothing cancellerRef

      case debouncerM of
        Nothing -> do
          -- NOTE debug
          H.liftEffect $ log "initialize debouncer"

          channel <- H.liftAff AVar.empty
          fiber <- scheduleDebounceSignal channel
          H.liftEffect $ Ref.write (Just { channel, fiber }) debouncerRef

          void $ H.fork do
            void <<< H.liftAff $ AVar.take channel
            H.liftEffect $ Ref.write Nothing debouncerRef

            -- NOTE Signal: Debouncer_Timer_Expired
            -- NOTE debug
            H.liftEffect $ log "debouncer timer expired, fork validation"

            let
              action :: FormValidationAction form
              action = unsafeCoerce variant

            canceller <- H.fork do
              -- NOTE debug
              H.liftEffect $ log "validation begins"

              eval $ H.action $ Validate action

              -- NOTE debug
              H.liftEffect $ log "validation ends"

            H.liftEffect $ Ref.write (Just canceller) cancellerRef

        Just { channel, fiber } -> do
          -- NOTE debug
          H.liftEffect $ log "renew debouncer"

          -- NOTE Field_Update -> renew debouncer
          interceptDebounceSignal fiber
          newFiber <- scheduleDebounceSignal channel
          H.liftEffect $ Ref.write (Just {channel, fiber: newFiber}) debouncerRef

    Reset variant a -> do
      -- NOTE debug
      H.liftEffect $ log "Reset"

      modifyState_ \st -> st
        { form = Internal.unsafeModifyInputVariant identity variant st.form
        , internal = over InternalState (_ { allTouched = false }) st.internal
        }
      eval $ SyncFormData a

    SetAll formInputs a -> do
      -- NOTE debug
      H.liftEffect $ log "SetAll"

      new <- modifyState \st -> st
        { form = Internal.replaceFormFieldInputs formInputs st.form }
      H.raise $ Changed $ getPublicState new
      eval $ SyncFormData a

    ModifyAll formInputs a -> do
      -- NOTE debug
      H.liftEffect $ log "ModifyAll"

      new <- modifyState \st -> st
        { form = Internal.modifyAll formInputs st.form }
      H.raise $ Changed $ getPublicState new
      eval $ SyncFormData a

    ValidateAll a -> do
      -- NOTE debug
      H.liftEffect $ log "ValidateAll"

      st <- getState
      form <- H.lift $ Internal.validateAll (unwrap st.internal).validators st.form
      modifyState_ _ { form = form }
      eval $ SyncFormData a

    -- A query to sync the overall state of the form after an individual field change
    -- or overall validation.
    SyncFormData a -> a <$ do
      -- NOTE debug
      H.liftEffect $ log "SyncFormData"

      st <- getState

      let errors = Internal.countErrors st.form
          dirty =
            (unwrap (Internal.formFieldsToInputFields st.form))
            /= (unwrap (unwrap st.internal).initialInputs)

      -- Need to verify the validity status of the form.
      newState <- if (unwrap st.internal).allTouched
        then modifyState _
          { validity = if st.errors /= 0 then Invalid else Valid
          , errors = errors
          , dirty = dirty
          }

        -- If not all fields are touched, then we need to quickly sync the form state
        -- to verify this is actually the case.
        else if Internal.allTouched st.form
          -- The sync revealed all fields really have been touched
          then modifyState _
            { validity = if st.errors /= 0 then Invalid else Valid
            , internal = over InternalState (_ { allTouched = true }) st.internal
            , errors = errors
            , dirty = dirty
            }

          -- The sync revealed that not all fields have been touched
          else modifyState _ { validity = Incomplete, errors = errors, dirty = dirty }

      H.raise $ Changed $ getPublicState newState

    -- Submit, also raising a message to the user
    Submit a -> a <$ do
      -- NOTE debug
      H.liftEffect $ log "Submit"

      mbForm <- runSubmit
      traverse_ (H.raise <<< Submitted) mbForm

    -- Submit, not raising a message
    SubmitReply reply -> do
      -- NOTE debug
      H.liftEffect $ log "SubmitReply"

      mbForm <- runSubmit
      pure $ reply mbForm

    -- | Should completely reset the form to its initial state
    ResetAll a -> a <$ do
      -- NOTE debug
      H.liftEffect $ log "ResetAll"

      new <- modifyState \st -> st
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form = Internal.replaceFormFieldInputs (unwrap st.internal).initialInputs st.form
        , internal = over InternalState (_ { allTouched = false }) st.internal
        }
      H.raise $ Changed $ getPublicState new

    GetState reply -> do
      -- NOTE debug
      H.liftEffect $ log "GetState"

      st <- getState
      pure $ reply $ getPublicState st

    Send cs cq -> do
      -- NOTE debug
      H.liftEffect $ log "Send"

      H.HalogenM $ liftF $ H.ChildQuery cs $ liftCoyoneda cq

    Raise query a -> a <$ do
      -- NOTE debug
      H.liftEffect $ log "Raise"

      H.raise (Emit query)

    LoadForm formInputs a -> a <$ do
      -- NOTE debug
      H.liftEffect $ log "LoadForm"

      st <- getState
      new <- modifyState _
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form = Internal.replaceFormFieldInputs formInputs st.form
        , internal = over
            InternalState
            (_
              { allTouched = false
              , initialInputs = formInputs
              }
            )
            st.internal
        }

      H.raise $ Changed $ getPublicState new

    Receive { render, validators } a -> a <$ do
      -- NOTE debug
      H.liftEffect $ log "Receive"

      let applyOver = over InternalState (_ { validators = validators })
      modifyStore_ render (\st -> st { internal = applyOver st.internal })

    AndThen q1 q2 a -> a <$ do
      -- NOTE debug
      H.liftEffect $ log "AndThen"

      void (eval q1)
      void (eval q2)

  -- Remove internal fields and return the public state
  getPublicState :: State form m -> PublicState form
  getPublicState = Record.delete (SProxy :: SProxy "internal")

  -- Run submission without raising messages or replies
  runSubmit :: DSL parent_query child_query child_slots form m (Maybe (FormOutputFields form))
  runSubmit = do
    init <- modifyState \st -> st
      { submitAttempts = st.submitAttempts + 1
      , submitting = true
      }

    -- For performance purposes, avoid running this if possible
    let internal = unwrap init.internal
    when (not internal.allTouched) do
      modifyState_ _
       { form = Internal.setFormFieldsTouched init.form
       , internal = over InternalState (_ { allTouched = true }) init.internal
       }

    -- Necessary to validate after fields are touched, but before parsing
    _ <- eval $ ValidateAll unit

    -- For performance purposes, only attempt to submit if the form is valid
    validated <- getState
    modifyState_ \st -> st { submitting = false }
    pure $
      if validated.validity == Valid
        then Internal.formFieldsToMaybeOutputFields validated.form
        else Nothing
