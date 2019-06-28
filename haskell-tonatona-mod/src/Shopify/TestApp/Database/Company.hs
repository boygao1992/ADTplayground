{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Shopify.TestApp.Database.Company where

import RIO
import Data.Time.LocalTime

import Database.Beam.Schema

data CompanyT f = Company
  { _id :: C f Word64
  , _name :: C f Text
  , _created_at :: C f LocalTime
  , _updated_at :: C f LocalTime
  } deriving (Generic, Beamable)
type Company = CompanyT Identity
deriving instance Eq Company
deriving instance Show Company

instance Table CompanyT where
  data PrimaryKey CompanyT f = CompanyId (C f Word64)
    deriving (Generic, Beamable)
  primaryKey = CompanyId . _id
type CompanyId = PrimaryKey CompanyT Identity
deriving instance Eq CompanyId
deriving instance Show CompanyId

Company
  (LensFor companyId)
  (LensFor companyName)
  (LensFor companyCreatedAt)
  (LensFor companyUpdatedAt)
  = tableLenses
