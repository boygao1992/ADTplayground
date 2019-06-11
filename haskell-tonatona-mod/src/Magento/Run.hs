{-# LANGUAGE TypeApplications #-}
module Magento.Run (run) where

import RIO
import Magento.Types
import Magento.Import.Api (Api, server)

import Tonatona.Servant.Run (runServantServer)

run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"

  runServantServer @Api server



