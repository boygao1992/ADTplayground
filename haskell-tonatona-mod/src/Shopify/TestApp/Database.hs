{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Shopify.TestApp.Database where

import RIO

import Database.Beam.Schema

import qualified Shopify.TestApp.Database.Oauth as O

data TestAppDb f = TestAppDb
  { _testappOauth :: f (TableEntity O.OauthT)
  } deriving (Generic, Database be)

testappDb :: DatabaseSettings be TestAppDb
testappDb = defaultDbSettings

TestAppDb
  (TableLens oauth)
  = dbLenses

