module FRP.MiniYampa.Types where

import Prelude

import Control.Monad.Rec.Class (Step (..), tailRecM3)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- | Time

type Time = Number
type DTime = Number

-- | Event
type Event = Maybe

-- | Signal Function (SF)
newtype SF a b = SF (DTime -> a -> { sf :: SF a b, output :: b })

instance semigroupoidSF :: Semigroupoid SF where
  compose :: forall a b c. SF b c -> SF a b -> SF a c
  compose (SF b2c) (SF a2b) = pac
    where
      pac = SF \dt a ->
        let
          a2br = a2b dt a
          b = a2br.output
          b2cr = b2c dt b
          c = b2cr.output
        in
          { sf: pac, output: c }

instance categorySF :: Category SF where
  identity :: forall a. SF a a
  identity = identitySF
    where
      identitySF = SF \_ x -> { sf: identitySF, output: x }

instance profunctorSF :: Profunctor SF where
  dimap :: forall s t a b. (s -> a) -> (b -> t) -> SF a b -> SF s t
  dimap f g (SF a2b) = pst
    where
      pst = SF \dt s ->
        let
          a = f s
          a2br = a2b dt a
          b = a2br.output
          t = g b
        in
          { sf: pst, output: t }

instance strongSF :: Strong SF where
  first :: forall a b c. SF a b -> SF (Tuple a c) (Tuple b c)
  first (SF f) = g
    where
      g = SF \dt (Tuple a c) ->
        let
          fr = f dt a
          b = fr.output
        in
          { sf: g, output: Tuple b c }

  second :: forall a b c. SF a b -> SF (Tuple c a) (Tuple c b)
  second (SF f) = g
    where
      g = SF \dt (Tuple c a) ->
        let
          fr = f dt a
          b = fr.output
        in
          { sf: g, output: Tuple c b }

runSF :: forall a b. SF a b -> DTime -> a -> { sf :: SF a b, output :: b }
runSF (SF sf) = sf

-- | Effectful Evaluation

newtype ReactState a b = ReactState
  { actuate :: Ref (ReactState a b) -> Boolean -> b -> Effect Boolean
  , sf :: SF a b
  , a :: a
  , b :: b
  }

reactInit
  :: forall a b
   . Effect a
  -> (Ref (ReactState a b)  -> Boolean -> b -> Effect Boolean)
  -> SF a b
  -> Effect (Ref (ReactState a b))
reactInit init actuate (SF a2b) = do
  a <- init
  let a2br = a2b 0.0 a -- first time interval of duration 0.0
      a2b' = a2br.sf
      b = a2br.output
  stateRef <- Ref.new (ReactState { actuate, sf: a2b', a, b })
  void $ actuate stateRef true b
  pure stateRef

react
  :: forall a b
   . Ref (ReactState a b)
  -> Tuple DTime (Maybe a)
  -> Effect Boolean
react rh (Tuple dt ma') = do
  (ReactState rs) <- Ref.read rh
  let a' = fromMaybe rs.a ma'
      r = (runSF rs.sf) dt a'
  Ref.write
    (ReactState rs { sf = r.sf, a = a', b = r.output })
    rh
  rs.actuate rh true r.output

reactimate
  :: forall a b
   . Effect a
  -> (Boolean -> Effect (Tuple DTime (Maybe a)))
  -> (Boolean -> b -> Effect Boolean)
  -> SF a b
  -> Effect Unit
reactimate init sense actuate (SF sf0) = do
  a0 <- init
  let r = sf0 0.0 a0
  tailRecM3 go r.sf a0 r.output
    where
      go (SF sf) a b = do
        done <- actuate true b
        if done
          then pure $ Done unit
          else do
            (Tuple dt ma') <- sense false
            let a' = fromMaybe a ma'
                r2 = sf dt a'
            pure $ Loop { a: r2.sf, b: a', c: r2.output }

-- | General signal functions
edge :: SF Boolean (Event Unit)
edge = SF (b2e true)
  where
    b2e
      :: Boolean
      -> DTime -> Boolean -> { sf :: SF Boolean (Event Unit)
                             , output :: Event Unit
                             }
    b2e prev dt curr =
      { sf : SF (b2e curr)
      , output:
        if not prev && curr
        then Just unit
        else Nothing
      }
