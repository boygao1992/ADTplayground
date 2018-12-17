module Generic.Eq where

import Prelude
import Generic

class GenericEq a where
  genericEq' :: a -> a -> Boolean

-- | terminals

instance genericEqNoConstructors :: GenericEq NoConstructors where
  genericEq' _ _ = true

instance genericEqNoArguments :: GenericEq NoArguments where
  genericEq' _ _ = true

-- | use existential Eq instance to actually evaluate equivalence
instance genericEqArgument :: Eq a => GenericEq (Argument a) where
  genericEq' (Argument x) (Argument y) = eq x y

-- | structural folding

-- | only one branch
instance genericEqConstructor :: GenericEq a => GenericEq (Constructor name a) where
  genericEq' (Constructor a1) (Constructor a2) = genericEq' a1 a2

-- | valid if one of the two branches is valid
instance genericEqSum
         :: (GenericEq a, GenericEq b)
         => GenericEq (Sum a b) where
           genericEq' (Inl a1) (Inl a2) = genericEq' a1 a2
           genericEq' (Inr b1) (Inr b2) = genericEq' b1 b2
           genericEq' _ _ = false

-- | valid if both branches are valid
instance genericEqProduct
  :: (GenericEq a, GenericEq b)
  => GenericEq (Product a b) where
    genericEq' (Product a1 b1) (Product a2 b2) =
      genericEq' a1 a2 && genericEq' b1 b2

genericEq :: forall a rep. Generic a rep => GenericEq rep => a -> a -> Boolean
genericEq x y = genericEq' (from x) (from y)
