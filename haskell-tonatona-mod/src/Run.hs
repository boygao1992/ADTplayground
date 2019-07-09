{-# LANGUAGE TypeApplications #-}
module Run (run) where

import RIO
import Types

run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"



