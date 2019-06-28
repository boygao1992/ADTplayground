{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Shopify.TestApp.Database where

import RIO

import Database.Beam.Schema
import Database.Beam.Migrate
import Database.Beam.Postgres

import qualified Shopify.TestApp.Database.Oauth as O
import qualified Shopify.TestApp.Database.Company as C
import qualified Shopify.TestApp.Database.User as U
import qualified Shopify.TestApp.Database.CompanyUser as CU

data TestAppDb f = TestAppDb
  { _testappOauth :: f (TableEntity O.OauthT)
  , _testappCompany :: f (TableEntity C.CompanyT)
  , _testappUser :: f (TableEntity U.UserT)
  , _testappCompanyUser :: f (TableEntity CU.CompanyUserT)
  } deriving (Generic, Database be)

testappDb :: DatabaseSettings be TestAppDb
testappDb = defaultDbSettings
  -- `withDbModification`
  -- dbModification
  -- { _testappCompanyUser = modifyTableFields tableModification
  --   { CU._company_id = C.CompanyId "company_id"
  --   , CU._user_id = U.UserId "user_id"
  --   }
  -- }

migratableDbSettings :: CheckedDatabaseSettings Postgres TestAppDb
migratableDbSettings = defaultMigratableDbSettings

TestAppDb
  (TableLens oauth)
  (TableLens company)
  (TableLens user)
  (TableLens companyUser)
  = dbLenses

