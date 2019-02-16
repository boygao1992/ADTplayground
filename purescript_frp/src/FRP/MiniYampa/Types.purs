module FRP.MiniYampa.Types where

import Prelude

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))

type Time = Number
type DTime = Number
data Event a = Event a | NoEvent
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


-- instance profunctor :: Profunctor

runSF :: forall a b. SF a b -> DTime -> a -> { sf :: SF a b, output :: b }
runSF (SF sf) = sf

