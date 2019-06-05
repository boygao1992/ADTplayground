{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Magento.Database.Eav.Attribute.Option.Value where

import RIO

import Database.Beam.Schema

import qualified Magento.Database.Eav.Attribute.Option as EavAttributeOption
import qualified Magento.Database.Store as Store

data EavAttributeOptionValueT f = EavAttributeOptionValue
  { _value_id :: C f Word32
  , _option_id :: PrimaryKey EavAttributeOption.EavAttributeOptionT f
  , _store_id :: PrimaryKey Store.StoreT f
  , _value :: C f (Maybe Text)
  } deriving (Generic, Beamable)
type EavAttributeOptionValue = EavAttributeOptionValueT Identity
deriving instance Eq EavAttributeOptionValue
deriving instance Show EavAttributeOptionValue

instance Table EavAttributeOptionValueT where
  data PrimaryKey EavAttributeOptionValueT f = ValueId (C f Word32)
    deriving (Generic, Beamable)
  primaryKey = ValueId . _value_id
type ValueId = PrimaryKey EavAttributeOptionValueT Identity
deriving instance Eq ValueId
deriving instance Show ValueId

EavAttributeOptionValue
  (LensFor value_id)
  (EavAttributeOption.OptionId (LensFor option_id))
  (Store.StoreId (LensFor store_id))
  (LensFor value)
  = tableLenses
