{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"
