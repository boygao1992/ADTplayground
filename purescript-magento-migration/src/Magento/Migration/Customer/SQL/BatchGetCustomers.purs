module Magento.Migration.Customer.SQL.BatchGetCustomers
( batchGetCustomers
) where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (CustomerFull)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchGetCustomers
  :: forall f r
  .  Foldable f
  => Functor f
  => Int
  -> f { entity_id :: Int | r }
  -> String
_batchGetCustomers website_id =
  ( \str ->
     F.format
      ( SProxy :: SProxy """
SELECT
    entity_id
  , updated_at
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
FROM customer_entity
WHERE website_id = {website_id} AND
( """)
      { website_id }
     <> str
     <> "\n)")
  <<< intercalate "\nOR"
  <<< map _customerCondition
  where
    _customerCondition :: { entity_id :: Int | r } -> String
    _customerCondition = F.format ( SProxy :: SProxy " entity_id=\"{entity_id}\" ")


batchGetCustomers
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> Int
  -> f { entity_id :: Int | r }
  -> Aff (Either String (Array CustomerFull))
batchGetCustomers conn website_id =
  (MySQL.query'_ <@> conn) <<< _batchGetCustomers website_id
