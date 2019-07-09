module Magento.Migration.Customer.SQL.BatchGetCustomerCore
( batchGetCustomerCore
)where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (CustomerCore)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchGetCustomerCore
  :: forall f r
     .  Foldable f
     => Functor f
     => Int
  -> f { email :: String | r }
  -> String
_batchGetCustomerCore website_id =
      ( \str ->
          F.format
            ( SProxy :: SProxy """
SELECT entity_id, website_id, email
FROM customer_entity
WHERE website_id = {website_id} AND
( """
            )
            { website_id }
        <> str
        <> "\n)")
  <<< intercalate "\nOR"
  <<< map _customerCondition
    where
      _customerCondition :: { email :: String | r} -> String
      _customerCondition = F.format ( SProxy :: SProxy " email=\"{email}\" ")

batchGetCustomerCore
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> Int
  -> f { email :: String | r }
  -> Aff (Either String (Array CustomerCore))
batchGetCustomerCore conn website_id =
  (MySQL.query'_ <@> conn) <<< _batchGetCustomerCore website_id
