module Magento.Migration.Deprecated.Customer.SQL.FullGetCustomerEntityId
( fullGetCustomerEntityId
) where

import Prelude

import Data.Either (Either)
import Effect.Aff (Aff)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_fullGetCustomerEntityId
  :: Int
  -> String
_fullGetCustomerEntityId website_id =
  F.format
    ( SProxy :: SProxy """
SELECT entity_id
FROM customer_entity
WHERE website_id = {website_id}
""")
    { website_id }

fullGetCustomerEntityId
  :: MySQL.Connection
  -> Int
  -> Aff (Either String (Array { entity_id :: Int }))
fullGetCustomerEntityId conn =
  (MySQL.query'_ <@> conn) <<< _fullGetCustomerEntityId
