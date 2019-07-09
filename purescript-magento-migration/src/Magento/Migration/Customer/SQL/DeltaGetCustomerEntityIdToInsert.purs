module Magento.Migration.Customer.SQL.DeltaGetCustomerEntityIdToInsert
( deltaGetCustomerEntityIdToInsert
) where

import Prelude

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_deltaGetCustomerEntityIdToInsert :: Int -> String
_deltaGetCustomerEntityIdToInsert website_id =
  F.format
    ( SProxy :: SProxy """
SELECT source_customer_id AS entity_id
FROM customer_entity_mapping
WHERE target_customer_id IS NULL
""")
    { website_id }

deltaGetCustomerEntityIdToInsert
  :: MySQL.Connection
  -> Int
  -> Aff (Either String (Array { entity_id :: Int }))
deltaGetCustomerEntityIdToInsert conn =
  (MySQL.query'_ <@> conn) <<< _deltaGetCustomerEntityIdToInsert
