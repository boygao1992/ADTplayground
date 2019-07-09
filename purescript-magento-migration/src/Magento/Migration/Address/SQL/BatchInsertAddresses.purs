module Magento.Migration.Address.SQL.BatchInsertAddresses
( batchInsertAddresses
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (AddressInsert)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchInsertAddresses
  :: forall f r
  .  Foldable f
  => Functor f
  => f (AddressInsert r)
  -> String
_batchInsertAddresses =
  ("""
INSERT INTO customer_address_entity
  ( parent_id
  , city
  , company
  , country_id
  , fax
  , firstname
  , lastname
  , middlename
  , postcode
  , prefix
  , region
  , region_id
  , street
  , suffix
  , telephone
  , uuid
  )
VALUES
""" <> _)
    <<< intercalate "\n,"
    <<< map value
  where
    value :: AddressInsert r -> String
    value =
      F.format ( SProxy :: SProxy " ({parent_id} , {city} , {company} , {country_id} , {fax} , {firstname} , {lastname} , {middlename} , {postcode} , {prefix} , {region} , {region_id} , {street} , {suffix} , {telephone} , \"{uuid}\")")

batchInsertAddresses
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f (AddressInsert r)
  -> Aff Unit
batchInsertAddresses conn =
  (MySQL.execute_ <@> conn) <<< _batchInsertAddresses
