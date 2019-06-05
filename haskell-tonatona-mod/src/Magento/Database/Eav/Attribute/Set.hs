{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Magento.Database.Eav.Attribute.Set where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Eav.Entity.Type as EavEntityType

data EavAttributeSetT f = EavAttributeSet
  { _attribute_set_id :: C f Word16
  , _entity_type_id :: PrimaryKey EavEntityType.EavEntityTypeT f
  , _attribute_set_name :: C f Text
  } deriving (Generic, Beamable)
type EavAttributeSet = EavAttributeSetT Identity
deriving instance Eq EavAttributeSet
deriving instance Show EavAttributeSet

instance Table EavAttributeSetT where
  data PrimaryKey EavAttributeSetT f = AttributeSetId (C f Word16)
    deriving (Generic, Beamable)
  primaryKey = AttributeSetId . _attribute_set_id
type AttributeSetId = PrimaryKey EavAttributeSetT Identity
deriving instance Eq AttributeSetId
deriving instance Show AttributeSetId

EavAttributeSet
  (LensFor attribute_set_id)
  (EavEntityType.EntityTypeId (LensFor entity_type_id))
  (LensFor attribute_set_name)
  = tableLenses
