module Main where

import Data.Fixed
import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

fromNumber' :: forall precision. KnownPrecision precision => Number -> PProxy precision -> Maybe (Fixed precision)
fromNumber' x _ = fromNumber x

test :: Maybe String
test = join $ reifyPrecision 3 (map toString <<< fromNumber' 123.4567)

main :: Effect Unit
main = do
  log "Hello sailor!"
