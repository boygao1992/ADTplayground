module Codec where

import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Writer (Writer)
import Data.Newtype (un)
import Data.Profunctor.Star (Star(..))

-- NOTE
-- asymmetric relation: a > b
data GCodec decodeM encodeM a b =
  GCodec
    (decodeM b)
    (Star encodeM a b) -- a -> encodeM b

{- State-based Formulation of Communication Process

-- model the alternating communication process
1. (current_System1_State x old_System2_State) x current_System2_State
-> (next_System1_State x current_System2_State) x next_System1_State
2. (old_System1_State x current_System2_State) x next_System1_State
-> (new_System1_State x next_System2_State) x next_System2_State
3. loop

1. Input(from System2) --decode-> current_System2_State,
   next_System1_State --encode-> Output(to System2)
2. Input(from System1) --decode-> next_System1_State,
   next_System1_State --encode-> Output(to System1)
-}

decoder :: forall m n a b. GCodec m n a b -> m b
decoder (GCodec d _) = d

encoder :: forall m n a b. GCodec m n a b -> a -> n b
encoder (GCodec _ e) = un Star e

newtype Codec m c d a b = Codec
  (GCodec
    (ReaderT c m)
    (Writer d)
    a b)

newtype BasicCodec m a b = BasicCodec (Codec m a a b b)
