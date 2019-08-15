module Type.Utils where

import Type.Data.Boolean as Bool

class IsEqualPred a b (o :: Bool.Boolean) | a b -> o

instance isEqualPredYes :: IsEqualPred a a Bool.True
else instance isEqualPredNo :: IsEqualPred a b Bool.False
