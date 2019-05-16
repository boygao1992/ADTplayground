{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

import Tonatona.Beam.MySQL.Run (runBeamMySQLDebug)
import DB
import Database.Beam.Query

run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"

  let sam = Person 1 "Sam" 10 :: Person

  (runBeamMySQLDebug
    . runInsert
    . insert (persistentDb ^. _persons)
    . insertValues
    $ [ sam ]) `catchAny` \(e :: SomeException) -> do
      logInfo $ displayShow e
