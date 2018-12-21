module Cont where

import Prelude

import ContT (ContT, runContT, mapContT, withContT)
import Data.Identity (Identity)
import Data.Newtype (unwrap, wrap)

-- | ContT r Identity a = ContT ((a -> Identity r) -> Identity r)
type Cont r = ContT r Identity

cont :: forall r a. ((a -> r) -> r) -> Cont r a
cont f = wrap $ \k -> wrap <<< f $ unwrap <<< k

runCont :: forall r a. Cont r a -> (a -> r) -> r
runCont c f = unwrap <<< runContT c $ wrap <<< f

mapCont :: forall r a. (r -> r) -> Cont r a -> Cont r a
-- mapCont f c = wrap $ \k -> unwrap c $ \a -> f <$> k a
mapCont f = mapContT ( wrap <<< f <<< unwrap )

withCont :: forall r a b. ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
-- compose wrap <<< f <<< compose unwrap :: ((b -> Identity r) -> (a -> Identity r))
-- compose unwrap :: (b -> Identity r) -> (b -> r)
                                 -- f :: ((b -> r) -> (a -> r))
                                   -- compose wrap :: (a -> r) -> (a -> Identity r)
withCont f = withContT ( compose wrap <<< f <<< compose unwrap)
