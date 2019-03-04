module Data.HObject.Primitive.Helpers where

import Data.HObject.Primitive (class Primitive)


-- | These are helper data types and instances so that you don't have to write out Primitive instances for your
-- | sum datatype consisting of primitive nullary types.

-- | These data types are alphabetically ordered by type.
-- | For example, a sum type of Int, Boolean and String would be BoolOrIntOrStr.

-- | Constructors are alphabetically ordered by the initials of the primitive types, and with the last letter
-- | as the type that it's constructing (ex. NSI consutrcts an Int type of IntOrNumOrStr)

data BoolOrInt = IB Boolean | BI Int
data BoolOrNum = NB Boolean | BN Number
data BoolOrStr = SB Boolean | BS String
data IntOrNum  = NI Int     | IN Number
data IntOrStr  = SI Int     | IS String
data NumOrStr  = SN Number  | NS String

data BoolOrIntOrNum = INB Boolean | BNI Int | BIN Number
data BoolOrNumOrStr = NSB Boolean | BSN Number | BNS String
data BoolOrIntOrStr = ISB Boolean | BSI Int | BIS String
data IntOrNumOrStr  = NSI Int | ISN Number | INS String

data Prim = PrimB Boolean | PrimI Int | PrimN Number | PrimS String


-- | Primitive instances
instance boolToBoolOrInt :: Primitive Boolean BoolOrInt where
  mkPrim = IB

instance intToBoolOrInt :: Primitive Int BoolOrInt where
  mkPrim = BI


instance boolToBoolOrNum :: Primitive Boolean BoolOrNum where
  mkPrim = NB

instance numToBoolOrNum :: Primitive Number BoolOrNum where
  mkPrim = BN


instance boolToBoolOrStr :: Primitive Boolean BoolOrStr where
  mkPrim = SB

instance strToBoolOrStr :: Primitive String BoolOrStr where
  mkPrim = BS


instance intToIntOrNum :: Primitive Int IntOrNum where
  mkPrim = NI

instance numToIntOrNum :: Primitive Number IntOrNum where
  mkPrim = IN


instance intToIntOrStr :: Primitive Int IntOrStr where
  mkPrim = SI

instance strToIntOrStr :: Primitive String IntOrStr where
  mkPrim = IS


instance numToNumOrStr :: Primitive Number NumOrStr where
  mkPrim = SN

instance strToNumOrStr :: Primitive String NumOrStr where
  mkPrim = NS




instance boolToBoolOrIntOrNum :: Primitive Boolean BoolOrIntOrNum where
  mkPrim = INB

instance intToBoolOrIntOrNum :: Primitive Int BoolOrIntOrNum where
  mkPrim = BNI

instance numToBoolOrIntOrNum :: Primitive Number BoolOrIntOrNum where
  mkPrim = BIN


instance boolToBoolOrNumOrStr :: Primitive Boolean BoolOrNumOrStr where
  mkPrim = NSB

instance numToBoolOrNumOrStr :: Primitive Number BoolOrNumOrStr where
  mkPrim = BSN

instance strToBoolOrNumOrStr :: Primitive String BoolOrNumOrStr where
  mkPrim = BNS


instance boolToBoolOrIntOrStr :: Primitive Boolean BoolOrIntOrStr where
  mkPrim = ISB

instance intToBoolOrIntOrStr :: Primitive Int BoolOrIntOrStr where
  mkPrim = BSI

instance strToBoolOrIntOrStr :: Primitive String BoolOrIntOrStr where
  mkPrim = BIS


instance intToIntOrNumOrStr :: Primitive Int IntOrNumOrStr where
  mkPrim = NSI

instance numToIntOrNumOrStr :: Primitive Number IntOrNumOrStr where
  mkPrim = ISN

instance strToIntOrNumOrStr :: Primitive String IntOrNumOrStr where
  mkPrim = INS




instance boolToPrim :: Primitive Boolean Prim where
  mkPrim = PrimB

instance intToPrim :: Primitive Int Prim where
  mkPrim = PrimI

instance numToPrim :: Primitive Number Prim where
  mkPrim = PrimN

instance strToPrim :: Primitive String Prim where
  mkPrim = PrimS
