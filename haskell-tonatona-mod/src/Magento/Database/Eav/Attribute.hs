{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Magento.Database.Eav.Attribute where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Eav.Entity.Type as EavEntityType

data EavAttributeT f = EavAttribute
  { _attribute_id :: C f Word16
  , _entity_type_id :: PrimaryKey EavEntityType.EavEntityTypeT f
  , _attribute_code :: C f (Maybe Text)
  , _backend_type :: C f (Maybe Text)
  , _frontend_input :: C f (Maybe Text)
  } deriving (Generic, Beamable)
type EavAttribute = EavAttributeT Identity
deriving instance Eq EavAttribute
deriving instance Show EavAttribute

instance Table EavAttributeT where
  data PrimaryKey EavAttributeT f = AttributeId (C f Word16)
    deriving (Generic, Beamable)
  primaryKey = AttributeId . _attribute_id
type AttributeId = PrimaryKey EavAttributeT Identity
deriving instance Eq AttributeId
deriving instance Show AttributeId

EavAttribute
  (LensFor attribute_id)
  (EavEntityType.EntityTypeId (LensFor entity_type_id))
  (LensFor attribute_code)
  (LensFor backend_type)
  (LensFor frontend_input)
  = tableLenses
