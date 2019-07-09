{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Shopify.TestApp.Database.CompanyUser where

import RIO
import Data.Time.LocalTime
import Database.Beam.Schema

import qualified Shopify.TestApp.Database.Company as Company
import qualified Shopify.TestApp.Database.User as User
import Shopify.TestApp.Data.Role (Role)

data CompanyUserT f = CompanyUser
  { _id :: C f Word64
  , _company_id :: PrimaryKey Company.CompanyT f
  , _user_id :: PrimaryKey User.UserT f
  , _role :: C f Role
  , _created_at :: C f LocalTime
  , _updated_at :: C f LocalTime
  } deriving (Generic, Beamable)
type CompanyUser = CompanyUserT Identity
deriving instance Eq CompanyUser
deriving instance Show CompanyUser

instance Table CompanyUserT where
  data PrimaryKey CompanyUserT f = CompanyUserId (C f Word64)
    deriving (Generic, Beamable)
  primaryKey = CompanyUserId . _id
type CompanyUserId = PrimaryKey CompanyUserT Identity
deriving instance Eq CompanyUserId
deriving instance Show CompanyUserId

CompanyUser
  (LensFor companyUserId)
  (Company.CompanyId (LensFor companyUserCompanyId))
  (User.UserId (LensFor companyUserUserId))
  (LensFor companyUserRole)
  (LensFor companyUserCreatedAt)
  (LensFor companyUserUpdatedAt)
  = tableLenses
