{-# LANGUAGE TypeApplications #-}
module Shopify.TestApp.Run (run) where

import RIO
import Shopify.TestApp.Types (Resources)
import Shopify.TestApp.Api (Api, server)

import Tonatona.Servant.Run (runServantServer)

run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"

  runServantServer @Api server



