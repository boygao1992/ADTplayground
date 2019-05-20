{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Magento.Database.Eav.Entity.Type where

import RIO

import Database.Beam.Schema

data EavEntityTypeT f = EavEntityType
  { _entity_type_id :: C f Word16
  , _entity_type_code :: C f Text
  , _entity_table :: C f (Maybe Text)
  } deriving (Generic, Beamable)
type EavEntityType = EavEntityTypeT Identity
deriving instance Eq EavEntityType
deriving instance Show EavEntityType

instance Table EavEntityTypeT where
  data PrimaryKey EavEntityTypeT f = EntityTypeId (C f Word16)
    deriving (Generic, Beamable)
  primaryKey = EntityTypeId . _entity_type_id
type EntityTypeId = PrimaryKey EavEntityTypeT Identity
deriving instance Eq EntityTypeId
deriving instance Show EntityTypeId

EavEntityType
  ( LensFor entity_type_id )
  ( LensFor entity_type_code )
  ( LensFor entity_table )
  = tableLenses
