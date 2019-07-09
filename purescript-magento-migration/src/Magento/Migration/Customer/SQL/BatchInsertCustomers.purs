module Magento.Migration.Customer.SQL.BatchInsertCustomers
( batchInsertCustomers
) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Magento.Migration.Types (CustomerInsert)
import Type.Data.Symbol (SProxy(..))

_batchInsertCustomers
  :: forall f r
  .  Foldable f
  => Functor f
  => f (CustomerInsert r)
  -> String
_batchInsertCustomers =
  ("""
INSERT INTO
  customer_entity
    ( website_id
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
""" <> _
  )
  <<< intercalate "\n,"
  <<< map value
  where
    value :: CustomerInsert r -> String
    value =
      F.format
        ( SProxy :: SProxy " ( {website_id} , \"{email}\" , {group_id} , {store_id} , {created_in} , {prefix} , {firstname} , {middlename} , {lastname} , {suffix} , {password_hash} , {gender})")


batchInsertCustomers
  :: forall f r
     .  Foldable f
     => Functor f
     => MySQL.Connection
  -> f (CustomerInsert r)
  -> Aff Unit
batchInsertCustomers conn =
  (MySQL.execute_ <@> conn) <<< _batchInsertCustomers
