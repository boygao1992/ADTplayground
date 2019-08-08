module Type.Data.Result where

import Prim.Symbol as Symbol

foreign import kind Result
foreign import data Failure :: Symbol -> Result
foreign import data Success :: Result

data RSProxy (result :: Result) = RSProxy

-- | AppendResult
class Append (r1 :: Result) (r2 :: Result) (r :: Result) | r1 r2 -> r

instance appendSS :: Append Success Success Success
instance appendSF :: Append Success (Failure err) (Failure err)
instance appendFS :: Append (Failure err) Success (Failure err)
instance appendFF ::
  ( Symbol.Append err1 " |> " err0
  , Symbol.Append err0 err2 err
  ) => Append (Failure err1) (Failure err2) (Failure err)
