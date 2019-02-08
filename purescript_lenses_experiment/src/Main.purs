module Main where

import Data.Lens (Lens, lens, view, over, set)
import Data.Lens.At (at)
import Data.Lens.Common (_1, _2)
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Tuple.Nested (Tuple4, tuple4)
import Effect (Effect)
import Effect.Console (log, logShow)
import Prelude

_action :: forall a b r. Lens { action :: a | r } { action :: b | r } a b
_action = lens _.action (_ { action = _ })

_tuple4
  :: forall a1 a2 a3 a4 b4
   . Lens (Tuple4 a1 a2 a3 a4) (Tuple4 a1 a2 a3 b4) a4 b4
_tuple4 = _2 <<< _2 <<< _2 <<< _1

main :: Effect Unit
main = do
  log "Hello sailor!"

  logShow $ view _action { action: "view" }
  logShow $ over _action (const "over") { action: "no action" }
  logShow $ set _action "set" { action: "no action"}

  logShow $ set _tuple4 0 (tuple4 1 2 3 4)

  logShow $ set (at "hello") (Just "world") Map.empty
  logShow $ view (at "hello") $ Map.fromFoldable [Tuple "hello" "world"]

  logShow $ view (at 1) $ Set.fromFoldable [1, 2, 3]
  logShow $ view (at 0) $ Set.fromFoldable [1, 2, 3]
