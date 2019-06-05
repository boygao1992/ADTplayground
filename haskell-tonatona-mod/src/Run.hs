{-# LANGUAGE TypeApplications #-}
module Run (run) where

import RIO
import Types
import Magento.Import.Api (Api, server)

import Tonatona.Servant.Run (runServantServer)

run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"

  runServantServer @Api server



