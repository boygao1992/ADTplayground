module Type.Data.Boolean.Utils where

import Type.Data.Boolean as Bool

class IsTrue (b :: Bool.Boolean)
instance isTrue :: IsTrue Bool.True

class IsFalse (b :: Bool.Boolean)
instance isFalse :: IsFalse Bool.False
