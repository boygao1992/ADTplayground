module Magento.Migration.Customer.SQL.FullInsertCustomerMappingSource
( fullInsertCustomerMappingSource
) where

import Prelude

import Effect.Aff (Aff)
import MySQL (Connection, execute_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_fullInsertCustomerMappingSource
  :: { created_at :: String
     , source_website_id :: Int
     }
  -> String
_fullInsertCustomerMappingSource =
  F.format
    ( SProxy :: SProxy """
INSERT IGNORE INTO customer_entity_mapping (source_customer_id, email, created_at)
  SELECT entity_id AS source_customer_id
         , email
         , "{created_at}" AS created_at
  FROM customer_entity
  WHERE website_id = {source_website_id}
""")



fullInsertCustomerMappingSource
  :: MySQL.Connection
  -> { source_website_id :: Int
     , created_at :: String
     }
  -> Aff Unit
fullInsertCustomerMappingSource conn =
  (MySQL.execute_ <@> conn ) <<< _fullInsertCustomerMappingSource
