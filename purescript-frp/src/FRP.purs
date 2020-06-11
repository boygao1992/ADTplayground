module FRP
  -- Combinator
  ( accum
  , foldl
  , sample
  , step
  , switch
  -- Behavior
  , Behavior
  -- Event
  , Event
  , mkEvent
  , subscribe
  -- Now
  , Now
  , runNow
  -- Interop
  , SinkEvent
  , sinkEvent
  ) where

import Prelude
import Data.Traversable as Data.Traversable
import Effect (Effect)
import Effect.Ref as Effect.Ref
import Foreign.Object as Foreign.Object

newtype Now a
  = Now (Effect a)

derive instance functorNow :: Functor Now

derive newtype instance applyNow :: Apply Now

derive newtype instance applicativeNow :: Applicative Now

derive newtype instance bindNow :: Bind Now

derive newtype instance monadNow :: Monad Now

runNow :: forall a. Now a -> Effect a
runNow (Now effect) = effect

newtype Behavior a
  = Behavior (Effect a)

derive newtype instance functorBehavior :: Functor Behavior

derive newtype instance applyBehavior :: Apply Behavior

derive newtype instance applicativeBehavior :: Applicative Behavior

derive newtype instance bindBehavior :: Bind Behavior

derive newtype instance monadBehavior :: Monad Behavior

newtype ABehavior event a
  = Sample (forall b. event (a -> b) -> event b)

sample :: forall a b. Behavior a -> Event (a -> b) -> Event b
sample (Behavior behaviorA) eventF =
  let
    subscribeB :: (b -> Effect Unit) -> Effect { unsubscribe :: Effect Unit }
    subscribeB continueB =
      let
        continueF :: (a -> b) -> Effect Unit
        continueF f = do
          a <- behaviorA
          (continueB <<< f) a
      in
        subscribe eventF continueF
  in
    mkEvent subscribeB

accum :: forall a b. (b -> a -> b) -> b -> Event a -> Now (Behavior b)
accum f initB eventA =
  Now do
    ref <- Effect.Ref.new initB
    _ <-
      subscribe eventA \a ->
        Effect.Ref.modify_ (f <@> a) ref
    pure $ Behavior (Effect.Ref.read ref)

foldl :: forall a b. (b -> a -> b) -> b -> Event a -> Now (Event b)
foldl f initB eventA = do
  behaviorB <- accum f initB eventA
  pure $ sample behaviorB (const identity <$> eventA)

step :: forall a. a -> Event a -> Now (Behavior a)
step init = accum const init

switch :: forall a. Behavior a -> Event (Behavior a) -> Now (Behavior a)
switch init = map join <<< step init

newtype Event a
  = Subscribe ((a -> Effect Unit) -> Effect { unsubscribe :: Effect Unit })

instance functorEvent :: Functor Event where
  map :: forall a b. (a -> b) -> Event a -> Event b
  map f event =
    mkEvent \continueB ->
      let
        continueA = continueB <<< f
      in
        subscribe event continueA

instance semigroupEvent :: Semigroup (Event a) where
  append :: forall a. Event a -> Event a -> Event a
  append event1 event2 =
    mkEvent \continue -> do
      e1 <- subscribe event1 continue
      e2 <- subscribe event2 continue
      pure { unsubscribe: e1.unsubscribe *> e2.unsubscribe }

instance monoidEvent :: Monoid (Event a) where
  mempty :: forall a. Event a
  mempty = mkEvent never
    where
    never _ = pure { unsubscribe: pure unit }

mkEvent ::
  forall a.
  ((a -> Effect Unit) -> Effect { unsubscribe :: Effect Unit }) ->
  Event a
mkEvent = Subscribe

subscribe ::
  forall a.
  Event a ->
  (a -> Effect Unit) ->
  Effect { unsubscribe :: Effect Unit }
subscribe (Subscribe e) = e

type SinkEvent a
  = { event :: Event a
    , push :: a -> Effect Unit
    }

sinkEvent :: forall a. Effect (SinkEvent a)
sinkEvent = do
  (subscribersRef :: Effect.Ref.Ref (Foreign.Object.Object (a -> Effect Unit))) <-
    Effect.Ref.new Foreign.Object.empty
  (newIdRef :: Effect.Ref.Ref Int) <-
    Effect.Ref.new 0
  let
    newId :: Effect String
    newId = map show $ Effect.Ref.modify (_ + 1) newIdRef

    subscribeA ::
      (a -> Effect Unit) ->
      Effect { unsubscribe :: Effect Unit }
    subscribeA continue = do
      id <- newId
      Effect.Ref.modify_ (Foreign.Object.insert id continue) subscribersRef
      pure
        { unsubscribe:
            Effect.Ref.modify_ (Foreign.Object.delete id) subscribersRef
        }

    push :: a -> Effect Unit
    push a = do
      subscribers <- Effect.Ref.read subscribersRef
      Data.Traversable.traverse_ (_ $ a) subscribers
  pure
    { event: mkEvent subscribeA
    , push
    }
