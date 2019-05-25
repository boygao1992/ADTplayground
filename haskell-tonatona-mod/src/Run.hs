{-# LANGUAGE TypeApplications #-}
module Run (run) where

import RIO
import Types
import Server (Api, server)

import Tonatona.Servant.Run (runServantServer)

run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"

  runServantServer @Api server



