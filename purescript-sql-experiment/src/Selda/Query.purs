module Selda.Query where

import Prelude
import Type.Prelude

import Data.Exists
import Data.Tuple.Nested
import Control.Monad.State
import Data.Traversable
import Data.Array ((:))
import Data.Array as Array
import Data.Either
import Data.Generic.Rep
import Data.Maybe

import Selda.Column
import Selda.Exp
import Selda.Generic
import Selda.Inner
import Selda.Query.Type
import Selda.SQL
import Selda.SQL as SQL
import Selda.SqlType (class SqlType)
import Selda.Table
import Selda.Transform


-- | Query the given table.
select :: forall s a. Table a -> Query s (Row s a)
select (Table {tableName: name, tableCols: cs}) = Query $ do
  rns <- renameAll $ map (\(ColInfo c) ->  c.colExpr) cs
  st <- get
  put $ st {sources = sqlFrom rns (TableName name) : st.sources }
  pure $ Many (map hideRenaming rns)

-- | Query an ad hoc table of type @a@. Each element in the given list represents
--   one row in the ad hoc table.
selectValues
  :: forall s a rep
  . Generic a rep
  => GRelation rep
  => Array a -> Query s (Row s a)
selectValues = Array.uncons >>> case _ of
  Nothing -> Query $ do
    st <- get
    put $ st { sources = sqlFrom [] EmptyTable : st.sources }
    pure $ Many $ gNew (Proxy :: Proxy rep)
  Just { head: row, tail: rows } -> Query $ do
    names <- traverse (const freshName) firstrow
    let rns = (\n -> named n (col n)) <$> names
        row' = mkFirstRow names
        rows' = map (map defToVal <<< params) rows
    s <- get
    put $ s { sources = sqlFrom rns (Values row' rows') : s.sources }
    pure $ Many $ map hideRenaming rns

    where
      firstrow = map defToVal $ params row

      mkFirstRow ns = do
      -- [ Named n (Lit l) | (Param l, n) <- zip firstrow ns ]
        (Param l /\ n) <- Array.zip firstrow ns
        pure $ runExists (named n <<< lit) l

      defToVal (Left x)  = x
      defToVal (Right x) = x
