module Type.Utils where

import Type.Data.Boolean as Bool

-- | IsEqualPred
-- https://github.com/justinwoo/purescript-type-isequal/
class IsEqualPred a b (o :: Bool.Boolean) | a b -> o

instance isEqualPredYes :: IsEqualPred a a Bool.True
else instance isEqualPredNo :: IsEqualPred a b Bool.False

-- | IsEqual
-- https://github.com/purescript/purescript-type-equality/
class IsEqual a b | a -> b, b -> a

instance isEqualImpl :: IsEqual a a

-- | IsIsomorphic
-- https://github.com/purescript/purescript-type-equality/
class IsIsomorphic a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a
