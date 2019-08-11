module Tensor where

import Prelude

import Data.Array as Array
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

newtype Matrix a = Matrix (Array (Array a))
derive instance functorMatrix :: Functor Matrix

instance showMatrix :: Show a => Show (Matrix a) where
  show (Matrix xs) =
    (\s -> "[\n" <> s <> "\n]")
    <<< Array.intercalate "\n"
    $ map showRow xs
    where
      showRow :: Show a => Array a -> String
      showRow = Array.intercalate " " <<< map show

outerProduct
  :: forall f g a b c
  . Functor f
  => Functor g
  => (a -> b -> c)
  -> f a -> g b -> f (g c)
outerProduct f xs ys = map (flip map ys <<< f) xs

switchL :: forall a b. b -> (a -> Array a -> b) -> Array a -> b
switchL n f xs = case Array.uncons xs of
  Nothing -> n
  Just { head, tail } -> f head tail

zipCons :: forall a. Array a -> Array (Array a) -> Array (Array a)
zipCons xt yt = case Array.uncons xt of
  Nothing -> yt
  Just { head: x, tail: xs } ->
    let (y /\ ys) = switchL ([] /\ []) (/\) yt
    in (x:y) : zipCons xs ys

zipConsSkew :: forall a. Array a -> Array (Array a) -> Array (Array a)
zipConsSkew xt yss = case Array.uncons xt of
  Nothing -> []:yss
  Just {head: x, tail: xs} -> [x] : zipCons xs yss

shearTranspose :: forall a. Array (Array a) -> Array (Array a)
shearTranspose = Array.foldr zipConsSkew []

tensorProduct
  :: forall a b c
  . (a -> b -> c)
  -> Matrix a
  -> Matrix b
  -> Matrix c
tensorProduct f (Matrix xs) (Matrix ys)
  = Matrix $ join $ outerProduct ((join <<< _) <<< outerProduct f) xs ys

toBoolMatrix :: Matrix Int -> Matrix Boolean
toBoolMatrix = map (\i -> i > 0)

tensorProductBool :: Matrix Boolean -> Matrix Boolean -> Matrix Boolean
tensorProductBool = tensorProduct (&&)

o2 :: Matrix Boolean
o2 = toBoolMatrix $ Matrix
  [[0,1,1]
  ,[0,0,0]
  ,[0,0,0]
  ]

i3 :: Matrix Boolean
i3 = toBoolMatrix $ Matrix
  [[0,0,0,1]
  ,[0,0,0,1]
  ,[0,0,0,1]
  ,[0,0,0,0]
  ]

exerciseX :: Matrix Boolean
exerciseX = toBoolMatrix $ Matrix
 [[0,1,0,0,0,0,0,0,0]
 ,[0,0,1,0,0,0,0,0,0]
 ,[0,0,0,1,0,0,0,0,0]
 ,[0,0,0,0,1,0,0,0,0]
 ,[0,0,1,0,0,0,0,0,0]
 ,[0,0,1,0,0,0,0,0,0]
 ,[0,0,0,1,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0,1]
 ,[0,0,0,0,0,0,0,1,0]
 ]

exerciseY :: Matrix Boolean
exerciseY = toBoolMatrix $ Matrix
  [[0,0,1,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,1,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,1,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,1,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,1,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,1,0,0,0,0,0,0]
  ,[0,0,0,1,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,1,0,0,0,0]
  ,[0,0,0,0,0,0,0,1,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,0,1,0]
  ,[0,0,0,0,0,0,0,0,0,0,0,0,1]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0]
  ]
