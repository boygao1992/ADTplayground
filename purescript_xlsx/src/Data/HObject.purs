module Data.HObject
  ( (-=)
  , (-<)
  , mkLeaf
  , mkBranch
  , mkTree
  , hJson
  , hObj
  , hObjToJson
  , TupleTree
  , HObject
  ) where

import Prelude (class Show, show, (<<<))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Tuple (Tuple(..))
import Data.Function.Uncurried (Fn2, runFn2)



infix 7 mkLeaf as -=
mkLeaf :: forall a. String -> a -> Tuple String (TupleTree a)
mkLeaf a b = Tuple a (Leaf b)

infix 7 mkBranch as -<
mkBranch :: forall a. String -> (Array (Tuple String (TupleTree a))) -> Tuple String (TupleTree a)
mkBranch a b = Tuple a (Branch b)

mkTree :: forall a. (Array (Tuple String (TupleTree a))) -> TupleTree a
mkTree a = Branch a

toObject :: forall a. TupleTree a -> HObject a
toObject = _toObject

toJson :: forall a. EncodeJson a => TupleTree a -> Json
toJson tupleTree = runFn2 _toJson tupleTree encodeJson

hJson :: forall a. EncodeJson a => (Array (Tuple String (TupleTree a))) -> Json
hJson = toJson <<< Branch

hObj :: forall a. (Array (Tuple String (TupleTree a))) -> HObject a
hObj = toObject <<< Branch



-- | Show instances
instance showHObject :: Show a => Show (HObject a) where
  show = serialize

serialize :: forall a. Show a => HObject a -> String
serialize hobj = runFn2 _serialize hobj show


instance showTupleTree :: Show a => Show (TupleTree a) where
  show (Leaf leaf) = show leaf
  show (Branch branch) = show branch



-- | If a homogenous record is needed but not used by the FFI, a TupleTree might work better since you can make use
-- | of the instances of the Tuple class
data TupleTree a = Leaf a | Branch (Array (Tuple String (TupleTree a)))

-- | The HObject type is provided for more complex types that are not meant to be JSON encoded but still need to be
-- | accessed by the FFI. It is interally represented as a homogenous JSON object
foreign import data HObject :: Type -> Type

foreign import _toJson :: forall a. Fn2 (TupleTree a) (a -> Json) Json

foreign import _toObject :: forall a. TupleTree a -> HObject a

foreign import _serialize :: forall a. Fn2 (HObject a) (a -> String) String

-- | Converts an hObject to Json
foreign import hObjToJson :: forall a. EncodeJson a => HObject a -> Json
