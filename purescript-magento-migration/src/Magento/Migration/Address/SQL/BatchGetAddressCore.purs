module Magento.Migration.Address.SQL.BatchGetAddressCore
( batchGetAddressCore
) where


import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Effect.Aff (Aff)
import Magento.Migration.Types (AddressCore)
import MySQL (Connection, query'_) as MySQL
import Record.Format (format) as F
import Type.Data.Symbol (SProxy(..))

_batchGetAddressCore
  :: forall f r
  .  Foldable f
  => Functor f
  => f { uuid :: String | r }
  -> String
_batchGetAddressCore =
  ("""
SELECT entity_id, uuid
FROM customer_address_entity
WHERE
""" <> _)
  <<< intercalate "\nOR"
  <<< map condition
  where
    condition :: { uuid :: String | r } -> String
    condition = F.format ( SProxy :: SProxy " uuid=\"{uuid}\" ")

batchGetAddressCore
  :: forall f r
  .  Foldable f
  => Functor f
  => MySQL.Connection
  -> f { uuid :: String | r }
  -> Aff (Either String (Array AddressCore))
batchGetAddressCore conn =
  (MySQL.query'_ <@> conn) <<< _batchGetAddressCore
