{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Magento.Database.Eav.Attribute.Option where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Eav.Attribute as EavAttribute

data EavAttributeOptionT f = EavAttributeOption
  { _option_id :: C f Word32
  , _attribute_id :: PrimaryKey EavAttribute.EavAttributeT f
  } deriving (Generic, Beamable)
type EavAttributeOption = EavAttributeOptionT Identity
deriving instance Eq EavAttributeOption
deriving instance Show EavAttributeOption

instance Table EavAttributeOptionT where
  data PrimaryKey EavAttributeOptionT f = OptionId (C f Word32)
    deriving (Generic, Beamable)
  primaryKey = OptionId . _option_id
type OptionId = PrimaryKey EavAttributeOptionT Identity
deriving instance Eq OptionId
deriving instance Show OptionId

EavAttributeOption
  (LensFor option_id)
  (EavAttribute.AttributeId (LensFor attribute_id))
  = tableLenses
