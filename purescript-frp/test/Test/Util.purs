module Test.Util
  ( spy
  , subscribeSpy
  ) where

import Prelude

import Data.Array as Data.Array
import Effect (Effect)
import Effect.Ref as Effect.Ref
import FRP as FRP

subscribeSpy ::
  forall a.
  FRP.Event a ->
  Effect
    { received :: Effect (Array a)
    , unsubscribe :: Effect Unit
    }
subscribeSpy event = do
  { callback, received } <- spy
  { unsubscribe } <- FRP.subscribe event callback
  pure
    { received
    , unsubscribe
    }

spy :: forall a. Effect { callback :: a -> Effect Unit, received :: Effect (Array a) }
spy = do
  receivedRef <- Effect.Ref.new []
  let
    callback :: a -> Effect Unit
    callback a = Effect.Ref.modify_ (_ `Data.Array.snoc` a) receivedRef

    received :: Effect (Array a)
    received = Effect.Ref.read receivedRef
  pure
    { callback
    , received
    }
