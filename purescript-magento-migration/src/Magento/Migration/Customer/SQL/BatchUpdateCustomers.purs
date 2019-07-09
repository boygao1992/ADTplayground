module Magento.Migration.Customer.SQL.BatchUpdateCustomers
( batchUpdateCustomers
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Magento.Migration.Types (CustomerUpdate)
import Type.Data.Symbol (SProxy(..))

_batchUpdateCustomers
  :: forall f r
  .  Foldable f
  => Functor f
  => f (CustomerUpdate r)
  -> String
_batchUpdateCustomers =
  (\str ->
    """
INSERT INTO
  customer_entity
    ( entity_id
    , website_id
    , email
    , group_id
    , store_id
    , created_in
    , prefix
    , firstname
    , middlename
    , lastname
    , suffix
    , password_hash
    , gender
    )
VALUES
""" <> str
    <> """
ON DUPLICATE KEY UPDATE
    website_id    = VALUES(website_id)
  , email         = VALUES(email)
  , group_id      = VALUES(group_id)
  , store_id      = VALUES(store_id)
  , created_in    = VALUES(created_in)
  , prefix        = VALUES(prefix)
  , firstname     = VALUES(firstname)
  , middlename    = VALUES(middlename)
  , lastname      = VALUES(lastname)
  , suffix        = VALUES(suffix)
  , password_hash = VALUES(password_hash)
  , gender        = VALUES(gender)
"""
  )
  <<< intercalate "\n,"
  <<< map value
  where
    value :: CustomerUpdate r -> String
    value =
      F.format
        ( SProxy :: SProxy " ({entity_id}, {website_id} , \"{email}\" , {group_id} , {store_id} , {created_in} , {prefix} , {firstname} , {middlename} , {lastname} , {suffix} , {password_hash} , {gender})")


batchUpdateCustomers
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f (CustomerUpdate r)
  -> Aff Unit
batchUpdateCustomers conn =
  (MySQL.execute_ <@> conn) <<< _batchUpdateCustomers
