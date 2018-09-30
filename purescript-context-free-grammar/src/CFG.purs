module CFG where

import Prelude
import Effect (Effect)
import Effect.Console as Console
import Control.Monad.Free (Free)

-- data S np vp
--   = NP_VP np vp

-- data PP np
--   = P_NP P np

-- data VP np pp vp
--   = V_NP V np
--   | VP_PP pp vp

-- data NP pp np
--   = D_N D N
--   | NP_PP np pp

type V = String
type P = String
type D = String
type N = String

data ExprF next
  = S next next
  | PP P next
  | VP_V_NP V next
  | VP_VP_PP next next
  | NP_D_N D N
  | NP_NP_P next next
derive instance functorExprF :: Functor(ExprF)

type Expr = Free ExprF

interpreter :: ExprF (Expr String) -> Expr String
interpreter = case _ of
  S next1 next2 -> do
    (\ x y -> "S ( " <> x <> y <> " )") <$> next1 <*> next2
  PP p next -> do
    (\ x -> "PP( " <> p <> x <> " )") <$> next
  VP_V_NP v next-> do
    (\ x -> "VP( " <> v <> x <> " )") <$> next
  VP_VP_PP next1 next2 -> do
    (\ x y -> "VP ( " <> x <> y <> " )") <$> next1 <*> next2
  NP_D_N d n -> do
    pure $ "NP (" <> d <> " " <> n <> " )"
  NP_NP_P next1 next2 -> do
    (\ x y -> "NP ( " <> x <> y <> " )") <$> next1 <*> next2

-- example :: Expr String
-- example = do
