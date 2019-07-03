module Formless.Component where

import Prelude

import Control.Monad.Free (liftF)
import Data.Eq (class EqRecord)
import Data.Functor.Variant as VF
import Data.Lens ((%=), (.=), use)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_, for_)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, match, inj, expand)
import Data.Variant.Internal (VariantRep(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Formless.Action as FA
import Formless.Data.FormFieldResult (FormFieldResult(..))
import Formless.Internal.Component as IC
-- import Formless.Internal.Debounce (debounceForm)
import Formless.Internal.Transform as IT
import Formless.Transform.Record (UnwrapField, unwrapOutputFields)
import Formless.Transform.Row (mkInputFields, class MakeInputFieldsFromRow)
import Formless.Types.Component (Action, Component, Debouncer, DebouncerField, Event(..), HalogenM, Input, InternalState(..), PublicAction, Query, QueryF(..), Spec, State, ValidStatus(..), _debouncerFieldM, _form)
import Formless.Types.Form (FormField, FormProxy(..), FormValidationAction, InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Halogen as H
import Halogen.HTML as HH
import Heterogeneous.Mapping as HM
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Unsafe.Coerce (unsafeCoerce)

-- | The default spec, which can be overridden by whatever functions you need
-- | to extend the component. For example:
-- |
-- | ```purescript
-- | mySpec = F.defaultSpec { render = myRender }
-- | ```
defaultSpec :: forall form st query act slots input msg m. Spec form st query act slots input msg m
defaultSpec =
  { render: const (HH.text mempty)
  , handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  , handleEvent: const (pure unit)
  , receive: const Nothing
  , initialize: Nothing
  , finalize: Nothing
  }

-- | A convenience function for raising a form's validated and unwrapped outputs
-- | as its only message to a parent component. Useful when you only want to be
-- | notified with a form's successfully-parsed data. For example:
-- |
-- | ```purescript
-- | type User = { name :: String, email :: Email }
-- |
-- | newtype UserForm r f = UserForm (r
-- |   ( name :: f Void String String
-- |   , email :: f EmailError String Email
-- |   ))
-- | derive instance newtypeUserForm :: Newtype (UserForm r f) _
-- |
-- | -- we only want to handle our `User` type on successful submission; we can
-- | -- use `raiseResult` as our `handleEvent` function to do this conveniently.
-- | formSpec = F.defaultSpec { handleEvent = raiseResult }
-- |
-- | -- the parent can now just handle the `User` output
-- | data ParentAction = HandleForm User
-- |
-- | type ChildSlots = ( formless :: F.Slot' UserForm User Unit )
-- | ```
raiseResult
  :: forall form st act slots wrappedOutput output m
  . Newtype (form Record OutputField) { | wrappedOutput }
  => HM.HMap UnwrapField { | wrappedOutput } { | output }
  => Event form st
  -> HalogenM form st act slots { | output } m Unit
raiseResult = case _ of
  Submitted out -> H.raise (unwrapOutputFields out)
  _ -> pure unit

-- | The Formless component, which takes a `spec` and provides a running form
-- | component from it.
component
  :: forall form st query act slots input msg m is ixs ivs fs fxs us vs os ifs ivfs
  . MonadAff m
  => RL.RowToList is ixs
  => RL.RowToList fs fxs
  => EqRecord ixs is
  => IT.InputFieldsToFormFields ixs is fs
  => IT.FormFieldsToInputFields fxs fs is
  => IT.CountErrors fxs fs
  => IT.AllTouched fxs fs
  => IT.SetFormFieldsTouched fxs fs fs
  => IT.ReplaceFormFieldInputs is fxs fs fs
  => IT.ModifyAll ifs fxs fs fs
  => IT.ValidateAll vs fxs fs fs m
  => IT.FormFieldToMaybeOutput fxs fs os
  => MakeInputFieldsFromRow ixs is is
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => Row.Lacks "validators" st
  => Row.Lacks "initialInputs" st
  => Row.Lacks "validity" st
  => Row.Lacks "dirty" st
  => Row.Lacks "errors" st
  => Row.Lacks "submitAttempts" st
  => Row.Lacks "submitting" st
  => Row.Lacks "form" st
  => Row.Lacks "internal" st
  => (input -> Input form st m)
  -> Spec form st query act slots input msg m
  -> Component form query slots input msg m
component mkInput spec = H.mkComponent
  { initialState: initialState <<< mkInput
  , render: IC.getPublicState >>> spec.render
  , eval: H.mkEval
      { handleQuery: \q -> handleQuery spec.handleQuery spec.handleEvent q
      , handleAction: \act -> handleAction spec.handleAction spec.handleEvent act
      , initialize: Just (inj (SProxy :: _ "initialize") spec.initialize)
      , receive: map (map FA.injAction) spec.receive
      , finalize: map FA.injAction spec.finalize
      }
  }
  where
  -- It's necessary to build from the original input because we have no idea
  -- what additional fields may have been provided by the user.
  initialState :: Input form st m -> State form st m
  initialState input = Builder.build pipeline input
    where
    initialInputs = case input.initialInputs of
      Nothing -> mkInputFields (FormProxy :: FormProxy form)
      Just inputs -> inputs
    initialForm = IT.inputFieldsToFormFields initialInputs
    internalState = InternalState
      { allTouched: false
      , initialInputs
      , validators: input.validators
      -- , debounceRef: Nothing -- TODO deprecate
      , debouncerFields: Map.empty
      , validationRef: Nothing
      }
    pipeline =
      Builder.delete (SProxy :: _ "validators")
        >>> Builder.delete (SProxy :: _ "initialInputs")
        >>> Builder.insert (SProxy :: _ "validity") Incomplete
        >>> Builder.insert (SProxy :: _ "dirty") false
        >>> Builder.insert (SProxy :: _ "errors") 0
        >>> Builder.insert (SProxy :: _ "submitAttempts") 0
        >>> Builder.insert (SProxy :: _ "submitting") false
        >>> Builder.insert (SProxy :: _ "form") initialForm
        >>> Builder.insert (SProxy :: _ "internal") internalState

handleAction
  :: forall form st act slots msg m is ixs ivs fs fxs us vs os ifs ivfs
  . MonadAff m
  => RL.RowToList is ixs
  => RL.RowToList fs fxs
  => EqRecord ixs is
  => IT.InputFieldsToFormFields ixs is fs
  => IT.FormFieldsToInputFields fxs fs is
  => IT.CountErrors fxs fs
  => IT.AllTouched fxs fs
  => IT.SetFormFieldsTouched fxs fs fs
  => IT.ReplaceFormFieldInputs is fxs fs fs
  => IT.ModifyAll ifs fxs fs fs
  => IT.ValidateAll vs fxs fs fs m
  => IT.FormFieldToMaybeOutput fxs fs os
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => Row.Lacks "internal" st
  => (act -> HalogenM form st act slots msg m Unit)
  -> (Event form st -> HalogenM form st act slots msg m Unit)
  -> Action form act
  -> HalogenM form st act slots msg m Unit
handleAction handleAction' handleEvent action = flip match action
  { initialize: \mbAction -> do
      -- DONE dr <- H.liftEffect $ Ref.new Nothing
      vr <- H.liftEffect $ Ref.new Nothing
      -- DONE let setFields = _ { debounceRef = Just dr, validationRef = Just vr }
      let setFields = _ { validationRef = Just vr }
      H.modify_ \st -> st { internal = over InternalState setFields st.internal }
      traverse_ handleAction' mbAction

  , syncFormData: \_ -> do
      st <- H.get
      let
        errors = IT.countErrors st.form
        dirty = not $ eq
          (unwrap (IT.formFieldsToInputFields st.form))
          (unwrap (unwrap st.internal).initialInputs)

      -- Need to verify the validity status of the form.
      newState <- case (unwrap st.internal).allTouched of
        true -> H.modify _
          { validity = if errors == 0 then Valid else Invalid
          , errors = errors
          , dirty = dirty
          }

        -- If not all fields are touched, then we need to quickly sync the form state
        -- to verify this is actually the case.
        _ -> case IT.allTouched st.form of

          -- The sync revealed all fields really have been touched
          true -> H.modify _
            { validity = if errors == 0 then Valid else Invalid
            , internal = over InternalState (_ { allTouched = true }) st.internal
            , errors = errors
            , dirty = dirty
            }

          -- The sync revealed that not all fields have been touched
          _ -> H.modify _ { validity = Incomplete, errors = errors, dirty = dirty }

      handleEvent $ Changed $ IC.getPublicState newState

  , userAction: \act ->
      handleAction' act

  , modify: \variant ->  do
      H.modify_ \st -> st
        { form = IT.unsafeModifyInputVariant identity variant st.form }
      handleAction handleAction' handleEvent sync

  , validate: \variant -> do
      st <- H.get
      let validators = (unwrap st.internal).validators
      form <- H.lift do
        IT.unsafeRunValidationVariant variant validators st.form
      H.modify_ _ { form = form }
      handleAction handleAction' handleEvent sync

  , modifyValidate: \(Tuple milliseconds variant) ->
      -- let
      --   modifyWith
      --     :: (forall e o. FormFieldResult e o -> FormFieldResult e o)
      --     -> HalogenM form st act slots msg m (form Record FormField)
      --   modifyWith f = do
      --     st <- H.modify \s -> s
      --       { form = IT.unsafeModifyInputVariant f variant s.form }
      --     pure st.form

      --   validate = do
      --     st <- H.get
      --     let vs = (unwrap st.internal).validators
      --     form <- H.lift do
      --       IT.unsafeRunValidationVariant (unsafeCoerce variant) vs st.form
      --     H.modify_ _ { form = form }
      --     pure form

      case milliseconds of
        Nothing -> do
          -- DONE
          -- modifyWith identity *> validate *> handleAction handleAction' handleEvent sync

          -- NOTE debug
          H.liftEffect $ log "ModifyValidate"

          let
            act :: FormValidationAction form
            act = unsafeCoerce variant -- NOTE it's ok to forget value types since only its label is used to pick out the correct validator from vs

          _form %= (IT.unsafeModifyInputVariant identity variant)
          handleAction handleAction' handleEvent (validateAction act)

        Just ms -> do
          -- DONE
          -- debounceForm
          --   ms
          --   (modifyWith identity)
          --   (modifyWith (const Validating) *> validate)
          --   (handleAction handleAction' handleEvent sync)

          -- NOTE debug
          H.liftEffect $ log "ModifyValidateAsync"

          _form %= (IT.unsafeModifyInputVariant identity variant)

          let
            label :: String
            label = case unsafeCoerce (unwrap variant) of
              VariantRep x -> x.type

          debouncerFieldM :: Maybe DebouncerField
            <- use (_debouncerFieldM label)

          debouncerField <- case debouncerFieldM of
            Nothing -> do
              debouncer <- H.liftEffect $ Ref.new Nothing
              canceller <- H.liftEffect $ Ref.new Nothing
              let debouncerField = { debouncer, canceller }
              (_debouncerFieldM label) .= (Just debouncerField)
              pure debouncerField
            Just df -> do
              pure df

          let
            debouncerRef :: Ref (Maybe Debouncer)
            debouncerRef = debouncerField.debouncer

            cancellerRef :: Ref (Maybe H.ForkId)
            cancellerRef = debouncerField.canceller

            scheduleDebounceSignal
              :: AVar.AVar Unit
              -> HalogenM form st act slots msg m (Aff.Fiber Unit)
            scheduleDebounceSignal channel = H.liftAff $ Aff.forkAff do
              Aff.delay ms
              AVar.put unit channel

            interceptDebounceSignal
              :: Aff.Fiber Unit
              -> HalogenM form st act slots msg m Unit
            interceptDebounceSignal =
              void <<< H.liftAff <<< Aff.killFiber (Aff.error "renew delayed signal")

          debouncerM <- H.liftEffect $ Ref.read debouncerRef
          cancellerM <- H.liftEffect $ Ref.read cancellerRef

          -- NOTE Field_Update -> intercept validation
          for_ cancellerM \canceller -> do
            -- NOTE debug
            H.liftEffect $ log "validation intercepted"

            H.kill canceller
            H.liftEffect $ Ref.write Nothing cancellerRef

          case debouncerM of
            Nothing -> do
              -- NOTE debug
              H.liftEffect $ log "initialize debouncer"

              channel <- H.liftAff AVar.empty
              fiber <- scheduleDebounceSignal channel
              H.liftEffect $ Ref.write <@> debouncerRef $ Just { channel, fiber }

              void $ H.fork do
                void <<< H.liftAff $ AVar.take channel
                H.liftEffect $ Ref.write Nothing debouncerRef

                -- NOTE Signal: Debouncer_Timer_Expired
                -- NOTE debug
                H.liftEffect $ log "debouncer timer expired, fork validation"

                let
                  act :: FormValidationAction form
                  act = unsafeCoerce variant

                canceller <- H.fork do
                  -- NOTE debug
                  H.liftEffect $ log "validation begins"

                  H.modify_ \s -> s { form = IT.unsafeModifyFormFieldResult (const Validating) variant s.form }
                  handleAction handleAction' handleEvent (validateAction act)

                  -- NOTE debug
                  H.liftEffect $ log "validation ends"

                H.liftEffect $ Ref.write <@> cancellerRef $ Just canceller

            Just { channel, fiber } -> do
              -- NOTE debug
              H.liftEffect $ log "renew debouncer"

              -- NOTE Field_Update -> renew debouncer
              interceptDebounceSignal fiber
              newFiber <- scheduleDebounceSignal channel
              H.liftEffect $ Ref.write <@> debouncerRef $ Just {channel, fiber: newFiber}

  , reset: \variant -> do
      H.modify_ \st -> st
        { form = IT.unsafeModifyInputVariant identity variant st.form
        , internal = over InternalState (_ { allTouched = false }) st.internal
        }
      handleAction handleAction' handleEvent sync

  , setAll: \(Tuple formInputs shouldValidate) -> do
      new <- H.modify \st -> st
        { form = IT.replaceFormFieldInputs formInputs st.form }
      handleEvent $ Changed $ IC.getPublicState new
      case shouldValidate of
        true -> handleAction handleAction' handleEvent FA.validateAll
        _ -> handleAction handleAction' handleEvent sync

  , modifyAll: \(Tuple formInputs shouldValidate) -> do
      new <- H.modify \st -> st
        { form = IT.modifyAll formInputs st.form }
      handleEvent $ Changed $ IC.getPublicState new
      case shouldValidate of
        true -> handleAction handleAction' handleEvent FA.validateAll
        _ -> handleAction handleAction' handleEvent sync

  , validateAll: \_ -> do
      st <- H.get
      form <- H.lift $ IT.validateAll (unwrap st.internal).validators st.form
      H.modify_ _ { form = form }
      handleAction handleAction' handleEvent sync

  , resetAll: \_ -> do
      new <- H.modify \st -> st
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form =
            IT.replaceFormFieldInputs (unwrap st.internal).initialInputs st.form
        , internal =
            over InternalState (_ { allTouched = false }) st.internal
        }
      handleEvent $ Changed $ IC.getPublicState new

  , submit: \_ -> do
      _ <- IC.preSubmit
      _ <- handleAction handleAction' handleEvent FA.validateAll
      IC.submit >>= traverse_ (Submitted >>> handleEvent)

  , loadForm: \formInputs -> do
      let setFields rec = rec { allTouched = false, initialInputs = formInputs }
      st <- H.get
      new <- H.modify _
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form = IT.replaceFormFieldInputs formInputs st.form
        , internal = over InternalState setFields st.internal
        }
      handleEvent $ Changed $ IC.getPublicState new
  }
  where
  sync :: Action form act
  sync = inj (SProxy :: SProxy "syncFormData") unit

  validateAction :: FormValidationAction form -> Action form act
  validateAction = inj (SProxy :: SProxy "validate")


handleQuery
  :: forall form st query act slots msg m a is ixs ivs fs fxs us vs os ifs ivfs
  . MonadAff m
  => RL.RowToList is ixs
  => RL.RowToList fs fxs
  => EqRecord ixs is
  => IT.InputFieldsToFormFields ixs is fs
  => IT.FormFieldsToInputFields fxs fs is
  => IT.CountErrors fxs fs
  => IT.AllTouched fxs fs
  => IT.SetFormFieldsTouched fxs fs fs
  => IT.ReplaceFormFieldInputs is fxs fs fs
  => IT.ModifyAll ifs fxs fs fs
  => IT.ValidateAll vs fxs fs fs m
  => IT.FormFieldToMaybeOutput fxs fs os
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => Row.Lacks "internal" st
  => (forall b. query b -> HalogenM form st act slots msg m (Maybe b))
  -> (Event form st -> HalogenM form st act slots msg m Unit)
  -> Query form query slots a
  -> HalogenM form st act slots msg m (Maybe a)
handleQuery handleQuery' handleEvent = VF.match
  { query: case _ of
      SubmitReply reply -> do
        _ <- IC.preSubmit
        _ <- handleAction (const (pure unit)) handleEvent FA.validateAll
        mbForm <- IC.submit
        pure $ Just $ reply mbForm

      SendQuery box ->
        H.HalogenM $ liftF $ H.ChildQuery box

      AsQuery (act :: Variant (PublicAction form)) a -> Just a <$
        handleAction
          (const (pure unit))
          handleEvent
          ((expand act) :: Action form act)

  , userQuery: \q -> handleQuery' q
  }
