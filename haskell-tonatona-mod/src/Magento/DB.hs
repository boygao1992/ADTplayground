module Magento.DB where

import RIO

import Database.Beam.Query
import Database.Beam.MySQL.Connection (MySQLM)

import Magento.Database
import qualified Magento.Database.Eav.Entity.Type as EET
import qualified Magento.Database.Eav.Attribute as EA
import qualified Magento.Database.Eav.Attribute.Option as EAO
import qualified Magento.Database.Eav.Attribute.Option.Value as EAOV

_getAllAttributeOptionValuesByAttributeCode :: String -> MySQLM [Maybe Text]
_getAllAttributeOptionValuesByAttributeCode my_attribute_code =
  runSelectReturningList $ select do
    eav_attribute_option_value <-
      filter_ (\table -> table^.EAOV.value /=. nothing_)
      $ all_ (magentoDb^.eavAttributeOptionValue)
    eav_attribute_option <- all_ (magentoDb^.eavAttributeOption)
    eav_attribute <- all_ (magentoDb^.eavAttribute)
    guard_ $ EAOV._option_id eav_attribute_option_value `references_` eav_attribute_option
    guard_ $ EAO._attribute_id eav_attribute_option `references_` eav_attribute
    guard_ $ eav_attribute^.EA.attribute_code ==. just_ (fromString my_attribute_code)
    pure $ eav_attribute_option_value^.EAOV.value

_getAttributeMetaData :: String -> String -> MySQLM (Maybe (EA.EavAttribute, EET.EavEntityType))
_getAttributeMetaData my_attribute_code my_entity_type_code =
  let
    attribute_code = fromString my_attribute_code
    entity_type_code = fromString my_entity_type_code
  in
    runSelectReturningOne
    $ select
    $ nub_ do
        eav_attribute <- all_ (magentoDb^.eavAttribute)
        eav_entity_type <- all_ (magentoDb^.eavEntityType)
        guard_ $ EA._entity_type_id eav_attribute `references_` eav_entity_type
        guard_ $ eav_attribute^.EA.attribute_code ==. just_ attribute_code
        guard_ $ eav_entity_type^.EET.entity_type_code ==. entity_type_code
        pure $
          ( eav_attribute
          , eav_entity_type
          )
