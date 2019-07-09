module Magento.Migration.Customer.SQL.DeltaGetCustomerEntityIdToUpdate
( deltaGetCustomerEntityIdToUpdate
) where

import Prelude

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_deltaGetCustomerEntityIdToUpdate :: Int -> String
_deltaGetCustomerEntityIdToUpdate website_id =
  F.format
    ( SProxy :: SProxy """
SELECT
    m.source_customer_id AS source_customer_id
  , m.target_customer_id AS target_customer_id
FROM customer_entity AS c
  LEFT join customer_entity_mapping AS m
  ON c.entity_id = m.source_customer_id
WHERE c.website_id = {website_id}
  AND m.target_customer_id IS NOT NULL
  AND c.updated_at > m.last_update;
""")
    { website_id }

deltaGetCustomerEntityIdToUpdate
  :: MySQL.Connection
  -> Int
  -> Aff (Either String (Array { source_customer_id :: Int
                               , target_customer_id :: Int
                               }))
deltaGetCustomerEntityIdToUpdate conn =
  (MySQL.query'_ <@> conn) <<< _deltaGetCustomerEntityIdToUpdate
