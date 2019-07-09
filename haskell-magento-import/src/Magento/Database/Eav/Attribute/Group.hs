{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Magento.Database.Eav.Attribute.Group where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Eav.Attribute.Set as EavAttributeSet

data EavAttributeGroupT f = EavAttributeGroup
  { _attribute_group_id :: C f Word16
  , _attribute_set_id :: PrimaryKey EavAttributeSet.EavAttributeSetT f
  , _attribute_group_name :: C f Text
  , _attribute_group_code :: C f Text
  , _tab_group_code :: C f (Maybe Text)
  } deriving (Generic, Beamable)
type EavAttributeGroup = EavAttributeGroupT Identity
deriving instance Eq EavAttributeGroup
deriving instance Show EavAttributeGroup

instance Table EavAttributeGroupT where
  data PrimaryKey EavAttributeGroupT f = AttributeGroupId (C f Word16)
    deriving (Generic, Beamable)
  primaryKey = AttributeGroupId . _attribute_group_id
type AttributeGroupId = PrimaryKey EavAttributeGroupT Identity
deriving instance Eq AttributeGroupId
deriving instance Show AttributeGroupId

EavAttributeGroup
  (LensFor attribute_group_id)
  (EavAttributeSet.AttributeSetId (LensFor attribute_set_id))
  (LensFor attribute_group_name)
  (LensFor attribute_group_code)
  (LensFor tab_group_code)
  = tableLenses
