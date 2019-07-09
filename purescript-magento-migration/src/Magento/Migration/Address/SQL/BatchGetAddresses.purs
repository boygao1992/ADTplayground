module Magento.Migration.Address.SQL.BatchGetAddresses
( batchGetAddresses
) where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (AddressFull)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchGetAddresses
  :: forall f r
  .  Foldable f
  => Functor f
  => f { entity_id :: Int | r }
  -> String
_batchGetAddresses =
  ("""
SELECT
    a.entity_id as entity_id
  , cm.target_customer_id as parent_id
  , a.city       AS city
  , a.company    AS company
  , a.country_id AS country_id
  , a.fax        AS fax
  , a.firstname  AS firstname
  , a.lastname   AS lastname
  , a.middlename AS middlename
  , a.postcode   AS postcode
  , a.prefix     AS prefix
  , a.region     AS region
  , a.region_id  AS region_id
  , a.street     AS street
  , a.suffix     AS suffix
  , a.telephone  AS telephone
  , a.updated_at AS updated_at
FROM customer_address_entity as a
  JOIN customer_entity_mapping as cm
    ON a.parent_id = cm.source_customer_id
WHERE
""" <> _)
  <<< intercalate "\nOR"
  <<< map condition
  where
    condition :: { entity_id :: Int | r } -> String
    condition = F.format ( SProxy :: SProxy " entity_id=\"{entity_id}\" ")

batchGetAddresses
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f { entity_id :: Int | r }
  -> Aff (Either String (Array AddressFull))
batchGetAddresses conn =
  (MySQL.query'_ <@> conn) <<< _batchGetAddresses
