-- | Renderless components use `Control.Comonad.Store` wrapped around
-- | their state type. This allows true renderless components, as the
-- | `extract` function from `Store` serves as the render function.
-- | However, this means that state update functions in the component's
-- | eval function are not actually operating on the component's state
-- | type; they're operating on the `Store` instead. This module contains
-- | helper functions to make this work easy to do.
module Renderless.State
( module Export
, getState
, getsState
, useState
, assignState
, modifyingState
, getRender
, modifyState
, modifyState_
, putState
, updateStore
, modifyStore
, modifyStore_
, putStore
) where

import Prelude

import Control.Comonad.Store (Store, runStore, seeks, store)
import Control.Monad.State (get, modify, modify_, put)
import Control.Monad.State.Class (class MonadState)
import Data.Lens (Getter, Setter, over, set, (^.))
import Data.Tuple (fst, snd)

import Control.Comonad.Store (Store, store) as Export
import Control.Comonad (extract) as Export

-- | We are working within the `State` monad as is always the case
-- | in Halogen. However, the state type we really want to access
-- | is within a further layer, `Store`. This function works as a
-- | drop-in for `H.get` to let you access your state record and
-- | use it as you ordinarily would.
-- |
-- | ```purescript
-- | -- Old
-- | st <- H.get
-- | -- New
-- | st <- getState
-- | ```
getState :: ∀ m state html. MonadState (Store state html) m => m state
getState = pure <<< snd <<< runStore =<< get

getsState :: forall m s h a. MonadState (Store s h) m => (s -> a) -> m a
getsState f = pure <<< f <<< snd <<< runStore =<< get

useState :: forall m h s t a b. MonadState (Store s h) m => Getter s t a b -> m a
useState p = getsState (_ ^. p)

assignState :: forall m h s a b. MonadState (Store s h) m => Setter s s a b -> b -> m Unit
assignState p b = void (modifyState (set p b))

modifyingState :: forall m h s a b. MonadState (Store s h) m => Setter s s a b -> (a -> b) -> m Unit
modifyingState p f = void (modifyState (over p f))

-- | You can also retrieve the render function, if you need to,
-- | from within the `Store` comonad.
-- |
-- | ```purescript
-- | renderFunction <- getRender
-- | ```
getRender :: ∀ m s h. MonadState (Store s h) m => m (s -> h)
getRender = pure <<< fst <<< runStore =<< get

-- | When you are modifying the state type, you need to apply a function
-- | (`state -> state`) within the `Store`. We can do this with the `seeks`
-- | function from `Control.Comonad.Store`. You could use this directly, or
-- | write helpers like the ones provided here.
-- |
-- | ```purescript
-- | -- without helpers
-- | H.modify_ $ seeks $ \st -> st { field = newValue }
-- | -- with helpers
-- | modifyState_ \st -> st { field = newValue }
-- | ```
modifyState :: ∀ m s h. MonadState (Store s h) m => (s -> s) -> m s
modifyState f = pure <<< snd <<< runStore =<< modify (seeks f)

modifyState_ :: ∀ m s h. MonadState (Store s h) m => (s -> s) -> m Unit
modifyState_ = modify_ <<< seeks

putState :: ∀ m s h. MonadState (Store s h) m => s -> m Unit
putState s = put <<< (store <@> s) =<< getRender

-- | In rare cases you will actually want to update the
-- | render function in `Store`. For those cases, you can use this
-- | convenient function along with `H.modify` to easily perform
-- | this kind of wholesale update:
-- |
-- | ```purescript
-- | H.modify $ updateStore (\st -> html) (\st -> st)
-- | ```
-- |
-- | You almost never need to use this except in your `Receiver`
-- | query, where it is necessary to update the render function
-- | with the new one passed via `Input`.
updateStore
  :: ∀ state html
  . (state -> html)
  -> (state -> state)
  -> Store state html
  -> Store state html
updateStore r f
  = store r <<< snd <<< runStore <<< seeks f

-- | You can also use these helper functions directly, rather than
-- | pass `updateStore` to `modify`.
-- |
-- | ```purescript
-- | newStore <- modifyStore render stateTransform
-- | putStore render state
-- | ```
modifyStore :: ∀ m s h. MonadState (Store s h) m => (s -> h) -> (s -> s) -> m (Store s h)
modifyStore r f = modify (updateStore r f)

modifyStore_ :: ∀ m s h. MonadState (Store s h) m => (s -> h) -> (s -> s) -> m Unit
modifyStore_ r f = modify_ (updateStore r f)

putStore :: ∀ m s h. MonadState (Store s h) m => (s -> h) -> s -> m Unit
putStore r s = put (store r s)
