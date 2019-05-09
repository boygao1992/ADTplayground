module Formless.Internal.Debounce where

-- import Prelude

-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (unwrap)
-- import Data.Traversable (traverse, traverse_)
-- import Effect.Aff (Error, Fiber, Milliseconds, delay, error, forkAff, killFiber)
-- import Effect.Aff.AVar (AVar)
-- import Effect.Aff.AVar as AVar
-- import Effect.Aff.Class (class MonadAff)
-- import Effect.Console (log)
-- import Effect.Ref (Ref)
-- import Effect.Ref as Ref
-- import Formless.Types.Component (DSL, Debouncer)
-- import Formless.Types.Form (FormField)
-- import Halogen as H
-- import Renderless.State (getState, modifyState_)

{-
newtype InternalState form m = InternalState
  { initialInputs :: form Record InputField
  , validators :: form Record (Validation form m)
  , allTouched :: Boolean
  , debounceRef :: Maybe (Ref (Maybe Debouncer)) -- NOTE
  , validationRef :: Maybe (Ref (Maybe (Error -> m Unit))) -- NOTE
  }
-}

-- | A helper function to debounce actions on the form and form fields. Implemented
-- | to reduce type variables necessary in the `State` type

-- debounceForm
--   :: forall pq cq cs form m a
--   . MonadAff m
--   => Milliseconds
--   -> DSL pq cq cs form m (form Record FormField)
--   -> DSL pq cq cs form m (form Record FormField)
--   -> DSL pq cq cs form m a
--   -> DSL pq cq cs form m Unit
-- debounceForm ms pre post last = do
--   state <- getState

--   let
--     dbRef :: Maybe (Ref (Maybe Debouncer))
--     dbRef = (unwrap state.internal).debounceRef

--     vdRef :: Maybe (Ref (Maybe (Error -> m Unit)))
--     vdRef = (unwrap state.internal).validationRef

--   -- if there is a running validation, cancel it
--   traverse_ (\f -> H.lift $ f $ error "times' up!") =<< readRef vdRef
--   debouncer <- readRef dbRef

--   case debouncer of
--     Nothing -> do
--       -- NOTE debug
--       H.liftEffect $ log "initialize debouncer"

--       var <- H.liftAff $ AVar.empty
--       fiber <- mkFiber var

--       _ <- H.fork do
--         _ <- H.liftAff (AVar.take var)
--         H.liftEffect $ traverse_ (Ref.write Nothing) dbRef
--         -- FIXME modified
--         atomic post (Just last)
--         -- atomic post Nothing

--       H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) dbRef

--       -- FIXME commented
--       atomic pre Nothing

--     Just db -> do
--       -- NOTE debug
--       H.liftEffect $ log "renew debouncer"

--       let var = db.var
--       void $ killFiber' db.fiber
--       fiber <- mkFiber var
--       H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) dbRef

--   where

--   mkFiber :: AVar Unit -> DSL pq cq cs form m (Fiber Unit)
--   mkFiber v = H.liftAff $ forkAff do
--     delay ms
--     AVar.put unit v

--   killFiber' :: forall x n. MonadAff n => Fiber x -> n Unit
--   killFiber' = H.liftAff <<< killFiber (error ("time's up!"))

--   readRef :: forall x n. MonadAff n => Maybe (Ref (Maybe x)) -> n (Maybe x)
--   readRef = H.liftEffect -- :: n (Maybe x)
--             <<< map join -- :: Effect (Maybe x)
--             <<< traverse Ref.read -- :: Effect (Maybe (Maybe x))

--   atomic
--     :: forall n
--     . MonadAff n
--     => DSL pq cq cs form n (form Record FormField)
--     -> Maybe (DSL pq cq cs form n a)
--     -> DSL pq cq cs form n Unit
--   atomic process maybeLast = do
--     state <- getState
--     let ref = (unwrap state.internal).validationRef
--     canceller <- readRef ref
--     traverse_
--       (\(f :: Error -> n Unit) -> do
--           H.liftEffect $ log "validation cancelled"
--           H.lift $ f $ error "new action"
--       )
--       canceller
--     -- NOTE commented
--     -- H.liftEffect $ traverse_ (Ref.write Nothing) ref

--     cancel <- H.fork do
--       -- NOTE debug
--       H.liftEffect $ log "validation forked"

--       form <- process
--       modifyState_ (_ { form = form })
--       H.liftEffect $ traverse_ (Ref.write Nothing) ref
--       traverse_ identity maybeLast

--     H.liftEffect $ traverse_ (Ref.write (Just cancel)) ref
--     pure unit
