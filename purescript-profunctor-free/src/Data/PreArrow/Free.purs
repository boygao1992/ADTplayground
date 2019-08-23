module Data.PreArrow.Free where

import Prelude

import Data.Exists1 (Exists, flippedRunExists, mkExists)
import Data.Profunctor (class Profunctor, lcmap, rmap)

-- NOTE type PreArrow p = (Profunctor p, Category p)
-- Free Weak Arrow
data FreePA p a b
  = Hom (a -> b)
  -- Lift (p a b)
  | Comp (Exists (CompF p a b)) -- forall x. p x b -> FreePA p a x -> FreePA p a b
data CompF p a b x = CompF (p x b) (FreePA p a x) -- TODO CatList
mkComp :: forall p a x b. p x b -> FreePA p a x -> FreePA p a b
mkComp pxb fax = Comp $ mkExists $ CompF pxb fax

instance profunctorFreePA :: Profunctor p => Profunctor (FreePA p) where
  dimap l r (Hom p) = Hom $ r <<< p <<< l
  dimap l r (Comp e) = flippedRunExists e \(CompF pxb fax) ->
    mkComp (rmap r pxb) (lcmap l fax)
instance semigroupoidFreePA :: Profunctor p => Semigroupoid (FreePA p) where
  compose :: forall a b c. FreePA p b c -> FreePA p a b -> FreePA p a c
  compose (Hom bc) fab = rmap bc fab
  compose (Comp e) fab = flippedRunExists e \(CompF pxc fbx) ->
    mkComp pxc (fbx <<< fab)
instance categoryFreePA :: Profunctor p => Category (FreePA p) where
  identity = Hom identity
