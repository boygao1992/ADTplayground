{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Client.Core.Suffix where

import GHC.TypeLits
import RIO
import qualified RIO.Text as Text

import Servant
import Servant.Client.Core as Client
import Servant.Client.Util (appendSuffixToPath)

data Suffix (l :: Symbol)

instance (KnownSymbol l, HasClient m api) => HasClient m (Suffix l :> api) where
  type Client m (Suffix l :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy api) (appendSuffixToPath (Text.pack . symbolVal $ Proxy @l) req)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl
