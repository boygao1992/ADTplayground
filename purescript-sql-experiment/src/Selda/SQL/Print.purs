module Selda.SQL.Print where

import Prelude

import Control.Monad.State (State, gets, modify_, runState)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Exists1 (flippedRunExists)
import Data.Exists2 (flippedRunExists2)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, sequence, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Selda.Exp (BinOp(..), BinOpF2(..), Exp(..), Fun2F2(..), InListF(..), NulOp(..), SomeCol(..), UnOp(..), UnOpF(..), runSomeExp)
import Selda.SQL (JoinType(..), Order(..), Param, SQL(..), SqlSource(..), mkParam, runParam)
import Selda.SQL.Print.Config (PPConfig(..))
import Selda.SqlType (LJustF(..), Lit(..), SqlTypeRep)
import Selda.Types as Types

snub :: forall a. Ord a => Array a -> Array a
snub = map NEA.head <<< Array.group <<< Array.sort

-- | SQL pretty-printer. The state is the list of SQL parameters to the
--   prepared statement.
type PP = State PPState

type PPState =
  { ppParams  :: Array Param
  , ppParamNS :: Int
  , ppQueryNS :: Int
  , ppConfig  :: PPConfig
  }

-- | Run a pretty-printer.
runPP
  :: PPConfig
  -> PP String
  -> String /\ Array Param
runPP cfg pp =
  let initState =
        { ppParams: []
        , ppParamNS: 1
        , ppQueryNS: 0
        , ppConfig: cfg
        }
  in (Array.reverse <<< _.ppParams) <$> runState pp initState

-- | Compile an SQL AST into a parameterized SQL query.
compSql :: PPConfig -> SQL -> (String /\ Array Param)
compSql cfg = runPP cfg <<< ppSql

-- | Compile a single column expression.
compExp :: forall a. PPConfig -> Exp SQL a -> (String /\ Array Param)
compExp cfg = runPP cfg <<< ppCol

-- | Compile an @UPATE@ statement.
compUpdate
  :: PPConfig
  -> Types.TableName
  -> Exp SQL Boolean
  -> Array (Types.ColName /\ SomeCol SQL)
  -> String /\ Array Param
compUpdate cfg tbl p cs = runPP cfg ppUpd
  where
    ppUpd :: PP String
    ppUpd = do
      updates <- traverse ppUpdate cs
      check <- ppCol p
      pure $ Array.fold
        [ "UPDATE", Types.fromTableName tbl
        , "SET", set updates
        , "WHERE", check
        ]
      where
        ppUpdate :: Types.ColName /\ SomeCol SQL -> PP (String \/ String)
        ppUpdate (n /\ c) = do
          let n' = Types.fromColName n
          c' <- ppSomeCol c
          let upd = Array.fold [n', "=", c']
          if n' == c'
            then pure $ Left upd
            else pure $ Right upd

        -- if the update doesn't change anything, pick an arbitrary column to
        -- set to itself just to satisfy SQL's syntactic rules
        set :: Array (String \/ String) -> String
        set us =
          let rights = Array.mapMaybe <@> us $ case _ of
                Right u -> Just u
                _ -> Nothing
              lefts = Array.mapMaybe <@> us $ case _ of
                Left u -> Just u
                _ -> Nothing
          in case rights of
            []  -> set (Right <$> Array.take 1 lefts)
            us' -> Array.intercalate ", " us'

-- | Compile a @DELETE@ statement.
compDelete :: PPConfig -> Types.TableName -> Exp SQL Boolean -> (String /\ Array Param)
compDelete cfg tbl p = runPP cfg ppDelete
  where
    ppDelete = do
      c' <- ppCol p
      pure $ Array.fold ["DELETE FROM", Types.fromTableName tbl, "WHERE", c']

-- | Pretty-print a literal as a named parameter and save the
--   name-value binding in the environment.
ppLit :: forall a. Lit a -> PP String
ppLit (LNull _)      = pure "NULL" -- NOTE proof ignored
ppLit (LJust lJustF) = flippedRunExists lJustF \(LJustF proof l) -> ppLit l -- NOTE proof ignored
ppLit l = do
  ps <- gets _.ppParams
  ns <- gets _.ppParamNS
  modify_ _
    { ppParams = mkParam l : ps
    , ppParamNS = ns + 1
    }
  pure $ "$" <> show ns

-- | Generate a unique name for a subquery.
freshQueryName :: PP String
freshQueryName = do
  qns <- gets _.ppQueryNS
  modify_ _ { ppQueryNS = qns + 1 }
  pure $ "q" <> show qns

-- | Pretty-print an SQL AST.
ppSql :: SQL -> PP String
ppSql (SQL sql) = do
  cs' <- traverse ppSomeCol sql.cols
  src' <- ppSrc sql.source
  r' <- ppRestricts sql.restricts
  gs' <- ppGroups sql.groups
  ord' <- ppOrder sql.ordering
  lim' <- ppLimit sql.limits
  pure $ Array.fold
    [ "SELECT ", if sql.distinct then "DISTINCT " else "", result cs'
    , src'
    , r'
    , gs'
    , ord'
    , lim'
    ]
  where
    ppSrc = case _ of
      EmptyTable -> do
        qn <- freshQueryName
        pure $ " FROM (SELECT NULL LIMIT 0) AS " <> qn
      TableName n -> do
        pure $ " FROM " <> Types.fromTableName n
      Product [] -> pure ""
      Product sqls -> do
        srcs <- traverse ppSql (Array.reverse sqls)
        qs <- for ((\s -> "(" <> s <> ")") <$> srcs) $ \q -> do
          qn <- freshQueryName
          pure (q <> " AS " <> qn)
        pure $ " FROM " <> Array.intercalate ", " qs
      Values row rows -> do
        row' <- Array.intercalate ", " <$> traverse ppSomeCol row
        rows' <- traverse ppRow rows
        qn <- freshQueryName
        pure $ Array.fold
          [ " FROM (SELECT "
          , Array.intercalate " UNION ALL SELECT " (row':rows')
          , ") AS "
          , qn
          ]
        where
          ppRow xs = do
            ls <- sequence $ runParam ppLit <$> xs
            pure $ Array.intercalate ", " ls
      Join jointype on left right -> do
        l' <- ppSql left
        r' <- ppSql right
        on' <- ppCol on
        lqn <- freshQueryName
        rqn <- freshQueryName
        pure $ Array.fold
          [ " FROM (", l', ") AS ", lqn
          , " ",  ppJoinType jointype, " (", r', ") AS ", rqn
          , " ON ", on'
          ]
        where
          ppJoinType LeftJoin  = "LEFT JOIN"
          ppJoinType InnerJoin = "JOIN"

    ppRestricts [] = pure ""
    ppRestricts rs = ppCols rs >>= \rs' -> pure $ " WHERE " <> rs'

    ppGroups [] = pure ""
    ppGroups grps = do
      cls <- sequence $ Array.mapMaybe <@> grps $ case _ of
        Some c -> Just $ runSomeExp c ppCol
        _ -> Nothing
      pure $ " GROUP BY " <> Array.intercalate ", " cls

    ppOrder [] = pure ""
    ppOrder os = do
      os' <- sequence $ Array.mapMaybe <@> os $ \(o /\ somecol) ->
        case somecol of
          Some c -> Just $ (_ <> (" " <> ppOrd o)) <$> runSomeExp c ppCol
          _ -> Nothing
      pure $ " ORDER BY " <> Array.intercalate ", " os'
      where
        ppOrd Asc = "ASC"
        ppOrd Desc = "DESC"

    ppLimit Nothing = pure ""
    ppLimit (Just (off /\ limit)) =
      pure $ " LIMIT " <> ppInt limit <> " OFFSET " <> ppInt off
      where
        ppInt = show

    result []  = "1"
    result cs' = Array.intercalate ", " cs'

ppSomeCol :: SomeCol SQL -> PP String
ppSomeCol (Some c)    = runSomeExp c ppCol
ppSomeCol (Named n c) = do
  c' <- runSomeExp c ppCol
  pure $ c' <> " AS " <> Types.fromColName n

ppCols :: Array (Exp SQL Boolean) -> PP String
ppCols cs = do
  cs' <- traverse ppCol (Array.reverse cs)
  pure $ "(" <> Array.intercalate ") AND (" cs' <> ")"

ppType :: SqlTypeRep -> PP String
ppType t = do
  (PPConfig c) <- gets _.ppConfig
  pure $ c.ppType t

ppTypePK :: SqlTypeRep -> PP String
ppTypePK t = do
  (PPConfig c) <- gets _.ppConfig
  pure $ c.ppTypePK t

ppCol :: forall a. Exp SQL a -> PP String
ppCol (Col name) = pure $ Types.fromColName name
ppCol (Lit l) = ppLit l
ppCol (BinOp binOpF2) = flippedRunExists2 binOpF2 \(BinOpF2 op a b) -> ppBinOp op a b
ppCol (UnOp unOpF) = flippedRunExists unOpF \(UnOpF op a) -> ppUnOp op a
ppCol (NulOp a) = ppNulOp a
ppCol (Fun2 fun2F2) = flippedRunExists2 fun2F2 \(Fun2F2 f a b) -> do
  a' <- ppCol a
  b' <- ppCol b
  pure $ Array.fold [f, "(", a', ", ", b', ")"]
ppCol (If a b c) = do
  a' <- ppCol a
  b' <- ppCol b
  c' <- ppCol c
  pure $ Array.fold ["CASE WHEN ", a', " THEN ", b', " ELSE ", c', " END"]
ppCol (Cast t exp) = runSomeExp exp \x -> do
  x' <- ppCol x
  t' <- ppType t
  pure $ Array.fold ["CAST(", x', " AS ", t', ")"]
ppCol (AggrEx f exp)   = runSomeExp exp (ppUnOp (Fun f))
ppCol (InList inListF) = flippedRunExists inListF \(InListF x xs) -> do
  x' <- ppCol x
  xs' <- traverse ppCol xs
  pure $ Array.fold [x', " IN (", Array.intercalate ", " xs', ")"]
ppCol (InQuery exp q) = runSomeExp exp \x -> do
  x' <- ppCol x
  q' <- ppSql q
  pure $ Array.fold [x', " IN (", q', ")"]

ppNulOp :: forall a. NulOp a -> PP String
ppNulOp (Fun0 f) = pure $ f <> "()"

ppUnOp :: forall a b. UnOp a b -> Exp SQL a -> PP String
ppUnOp op c = do
  c' <- ppCol c
  pure $ case op of
    Abs    -> "ABS(" <> c' <> ")"
    Sgn    -> "SIGN(" <> c' <> ")"
    Neg    -> "-(" <> c' <> ")"
    Not    -> "NOT(" <> c' <> ")"
    IsNull -> "(" <> c' <> ") IS NULL"
    Fun f  -> f <> "(" <> c' <> ")"

ppBinOp :: forall a b c. BinOp a b c -> Exp SQL a -> Exp SQL b -> PP String
ppBinOp op a b = do
  a' <- ppCol a
  b' <- ppCol b
  pure $ paren a a' <> " " <> ppOp op <> " " <> paren b b'
  where
    paren :: forall d. Exp SQL d -> String -> String
    paren (Col _) c = c
    paren (Lit _) c = c
    paren _ c       = "(" <> c <> ")"

    ppOp :: BinOp a b c -> String
    ppOp Gt   = ">"
    ppOp Lt   = "<"
    ppOp Gte  = ">="
    ppOp Lte  = "<="
    ppOp Eq   = "="
    ppOp Neq  = "!="
    ppOp And  = "AND"
    ppOp Or   = "OR"
    ppOp Add  = "+"
    ppOp Sub  = "-"
    ppOp Mul  = "*"
    ppOp Div  = "/"
    ppOp Like = "LIKE"
    ppOp (CustomOp s) = s

