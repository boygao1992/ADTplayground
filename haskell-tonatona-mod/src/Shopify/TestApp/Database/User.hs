{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Shopify.TestApp.Database.User where

import RIO
import Data.Time.LocalTime

import Database.Beam.Schema

data UserT f = User
  { _id :: C f Word64
  , _first_name :: C f (Maybe Text)
  , _last_name :: C f (Maybe Text)
  , _created_at :: C f LocalTime
  , _updated_at :: C f LocalTime
  } deriving (Generic, Beamable)
type User = UserT Identity
deriving instance Eq User
deriving instance Show User

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f Word64)
    deriving (Generic, Beamable)
  primaryKey = UserId . _id
type UserId = PrimaryKey UserT Identity
deriving instance Eq UserId
deriving instance Show UserId

User
  (LensFor userId)
  (LensFor userFirstName)
  (LensFor userLastName)
  (LensFor userCreatedAt)
  (LensFor userUpdatedAt)
  = tableLenses
