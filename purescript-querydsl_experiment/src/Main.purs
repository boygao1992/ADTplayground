module Main where

import Prelude

import QueryDsl (Column, SelectQuery, Table, asc, from, limit, makeTable, orderBy, select, toSql, where_, join)
import Effect (Effect)
import Effect.Console (logShow)
import QueryDsl.Expressions ((:==))
import Type.Data.Boolean (False, True)

customer = makeTable "customer" :: Table
  ( id :: Column Int False
  , firstName :: Column String True
  , lastName :: Column String True
  )

order = makeTable "order" :: Table
  ( id :: Column Int False
  , customer_id :: Column Int True
  , total :: Column Int True
  )

getLastNamesQuery :: SelectQuery ( lastName :: String )
getLastNamesQuery = do
  c <- from customer
  pure $ select { lastName : c.lastName }
    `where_` (c.firstName :== "Bob" )
    `orderBy` [asc c.id]
    `limit` 10

joinCustomerOrder :: SelectQuery ( name :: String, total :: Int )
joinCustomerOrder = do
  c <- from customer
  o <- join order (\o -> o.customer_id :== c.id)
  pure $ select { name: c.firstName, total: o.total }


main :: Effect Unit
main = do
  logShow $ toSql getLastNamesQuery
  logShow $ toSql joinCustomerOrder
  pure unit

