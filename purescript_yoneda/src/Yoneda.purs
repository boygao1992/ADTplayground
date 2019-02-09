module Yoneda where

import Prelude hiding (type (~>))

-- | [Bartosz Milewski - Profunctor Optics](https://www.youtube.com/watch?v=l1FCXUi6Vlw&t=939s)

type NaturalTransformation (f :: Type -> Type)
                           (g :: Type -> Type)
  = forall a. f a -> g a
infixr 4 type NaturalTransformation as ~>

type HigherNaturalTransformation (u :: (Type -> Type) -> Type)
                                 (v :: (Type -> Type) -> Type)
  = forall w. u w -> v w
infixr 4 type HigherNaturalTransformation as ~~>


-- Reader = Function
type Reader a x = a -> x
 -- forall x. Reader a x -> f x
type Yo f a = Reader a ~> f

-- Yoneda Lemma
toYo :: forall f a. Functor f => f a -> Yo f a
                                     -- Yo f a :: forall x. (a -> x) -> f x
        -- a2x :: forall x. a -> x
               -- map a2x :: f a -> f x
               -- map a2x fa :: f x
       -- \a2x -> map a2x fa :: forall x. (a -> x) -> f x
toYo fa = \a2x -> map a2x fa

fromYo :: forall f a. Functor f => Yo f a -> f a
    -- alpha :: forall x. (a -> x) -> f x
            -- alpha :: (a -> a) -> f a
                  -- identity :: a -> a
            -- alpha identity :: f a
fromYo alpha = alpha identity

-- Yoneda Embedding
toYoE :: forall a b. (b -> a) -> (Reader a ~> Reader b)
toYoE = toYo

fromYoE :: forall a b. (Reader a ~> Reader b) -> (b -> a)
fromYoE = fromYo

-- Yoneda on Functors

toYoEF
  :: forall f g
   . (g ~> f) -> (NaturalTransformation f ~~> NaturalTransformation g)
toYoEF = toYo

fromYoEF
  :: forall f g
   . Functor f
  => (NaturalTransformation f ~~> NaturalTransformation g) -> (g ~> f)
fromYoEF beta = beta identity

-- | purescript-free, Data.Yoneda
newtype Yoneda f a = Yoneda (Reader a ~> f)

liftYoneda :: forall f a. Functor f => f a -> Yoneda f a
liftYoneda fa = Yoneda (\a2x -> map a2x fa)

lowerYoneda :: forall f a. Yoneda f a -> f a
lowerYoneda (Yoneda alpha) = alpha identity
