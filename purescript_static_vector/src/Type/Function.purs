module Type.Function where

import Type.Data.Boolean as Bool

-- | IsFunctionPred
class IsFunctionPred f (b :: Bool.Boolean) | f -> b

instance isFunctionPredYes :: IsFunctionPred (i -> o) Bool.True
else instance isFunctionPredNo :: IsFunctionPred a Bool.False

-- | Cons

class Cons a b f | f -> a b

instance consFunc ::
  Cons a b (a -> b)
