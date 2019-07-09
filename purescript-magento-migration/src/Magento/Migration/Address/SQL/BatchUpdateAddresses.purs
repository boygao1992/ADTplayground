module Magento.Migration.Address.SQL.BatchUpdateAddresses
( batchUpdateAddresses
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (AddressUpdate)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchUpdateAddresses
  :: forall f r
  .  Foldable f
  => Functor f
  => f (AddressUpdate r)
  -> String
_batchUpdateAddresses =
  (\str -> """
INSERT INTO customer_address_entity
  ( entity_id
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
  )
VALUES
""" <> str
    <> """
ON DUPLICATE KEY UPDATE
    city       = VALUES(city)
  , company    = VALUES(company)
  , country_id = VALUES(country_id)
  , fax        = VALUES(fax)
  , firstname  = VALUES(firstname)
  , lastname   = VALUES(lastname)
  , middlename = VALUES(middlename)
  , postcode   = VALUES(postcode)
  , prefix     = VALUES(prefix)
  , region     = VALUES(region)
  , region_id  = VALUES(region_id)
  , street     = VALUES(street)
  , suffix     = VALUES(suffix)
  , telephone  = VALUES(telephone)
""")
    <<< intercalate "\n,"
    <<< map value
  where
    value :: AddressUpdate r -> String
    value =
      F.format ( SProxy :: SProxy " ({entity_id} , {city} , {company} , {country_id} , {fax} , {firstname} , {lastname} , {middlename} , {postcode} , {prefix} , {region} , {region_id} , {street} , {suffix} , {telephone})")

batchUpdateAddresses
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f (AddressUpdate r)
  -> Aff Unit
batchUpdateAddresses conn =
  (MySQL.execute_ <@> conn) <<< _batchUpdateAddresses
