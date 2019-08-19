module Data.Generic.Rep.Lens.Constraints where

import Prelude

import Data.Lens

newtype COptic s t a b = COptic (forall p. Optic p s t a b)
newtype CIso s t a b = CIso (Iso s t a b)
newtype CLens s t a b = CLens (Lens s t a b)
newtype CPrism s t a b = CPrism (Prism s t a b)
-- TODO AffineTraversal
newtype CTraversal s t a b = CTraversal (Traversal s t a b)

unwrapOptic :: forall s t a b. COptic s t a b -> (forall p. Optic p s t a b)
unwrapOptic (COptic l) = l
unwrapIso :: forall s t a b. CIso s t a b -> Iso s t a b
unwrapIso (CIso l) = l
unwrapLens :: forall s t a b. CLens s t a b -> Lens s t a b
unwrapLens (CLens l) = l
unwrapPrism :: forall s t a b. CPrism s t a b -> Prism s t a b
unwrapPrism (CPrism l) = l
unwrapTraversal :: forall s t a b. CTraversal s t a b -> Traversal s t a b
unwrapTraversal (CTraversal l) = l

-- foreign import kind Constraint
-- foreign import data CProfunctor :: Constraint
-- foreign import data CStrong :: Constraint
-- foreign import data CChoice :: Constraint
-- foreign import data CWander :: Constraint

class ComposeLenses
  (lens1 :: Type -> Type -> Type -> Type -> Type)
  (lens2 :: Type -> Type -> Type -> Type -> Type)
  (lensO :: Type -> Type -> Type -> Type -> Type)
  | lens1 lens2 -> lensO
  where
    composeLenses :: forall u v s t a b. lens1 u v s t -> lens2 s t a b -> lensO u v a b

infixr 7 composeLenses as .<<<

-- Optic <<< ?
instance composeLensesCOpticCOptic ::
  ComposeLenses COptic COptic COptic where
  composeLenses (COptic l1) (COptic l2) = COptic (l1 <<< l2)
instance composeLensesCOpticCIso ::
  ComposeLenses COptic CIso CIso where
  composeLenses (COptic l1) (CIso l2) = CIso (l1 <<< l2)
instance composeLensesCOpticCLens ::
  ComposeLenses COptic CLens CLens where
  composeLenses (COptic l1) (CLens l2) = CLens (l1 <<< l2)
instance composeLensesCOpticCPrism ::
  ComposeLenses COptic CPrism CPrism where
  composeLenses (COptic l1) (CPrism l2) = CPrism (l1 <<< l2)
instance composeLensesCOpticCTraversal ::
  ComposeLenses COptic CTraversal CTraversal where
  composeLenses (COptic l1) (CTraversal l2) = CTraversal (l1 <<< l2)

-- Iso <<< ?
instance composeLensesCIsoCOptic ::
  ComposeLenses CIso COptic CIso where
  composeLenses (CIso l1) (COptic l2) = CIso (l1 <<< l2)
instance composeLensesCIsoCIso ::
  ComposeLenses CIso CIso CIso where
  composeLenses (CIso l1) (CIso l2) = CIso (l1 <<< l2)
instance composeLensesCIsoCLens ::
  ComposeLenses CIso CLens CLens where
  composeLenses (CIso l1) (CLens l2) = CLens (l1 <<< l2)
instance composeLensesCIsoCPrism ::
  ComposeLenses CIso CPrism CPrism where
  composeLenses (CIso l1) (CPrism l2) = CPrism (l1 <<< l2)
instance composeLensesCIsoCTraversal ::
  ComposeLenses CIso CTraversal CTraversal where
  composeLenses (CIso l1) (CTraversal l2) = CTraversal (l1 <<< l2)

-- Lens <<< ?
instance composeLensesCLensCOptic ::
  ComposeLenses CLens COptic CLens where
  composeLenses (CLens l1) (COptic l2) = CLens (l1 <<< l2)
instance composeLensesCLensCIso ::
  ComposeLenses CLens CIso CLens where
  composeLenses (CLens l1) (CIso l2) = CLens (l1 <<< l2)
instance composeLensesCLensCLens ::
  ComposeLenses CLens CLens CLens where
  composeLenses (CLens l1) (CLens l2) = CLens (l1 <<< l2)
instance composeLensesCLensCPrism ::
  ComposeLenses CLens CPrism CTraversal where
  composeLenses (CLens l1) (CPrism l2) = CTraversal (l1 <<< l2)
instance composeLensesCLensCTraversal ::
  ComposeLenses CLens CTraversal CTraversal where
  composeLenses (CLens l1) (CTraversal l2) = CTraversal (l1 <<< l2)

-- Prism <<< ?
instance composeLensesCPrismCOptic ::
  ComposeLenses CPrism COptic CPrism where
  composeLenses (CPrism l1) (COptic l2) = CPrism (l1 <<< l2)
instance composeLensesCPrismCIso ::
  ComposeLenses CPrism CIso CPrism where
  composeLenses (CPrism l1) (CIso l2) = CPrism (l1 <<< l2)
instance composeLensesCPrismCLens ::
  ComposeLenses CPrism CLens CTraversal where
  composeLenses (CPrism l1) (CLens l2) = CTraversal (l1 <<< l2)
instance composeLensesCPrismCPrism ::
  ComposeLenses CPrism CPrism CPrism where
  composeLenses (CPrism l1) (CPrism l2) = CPrism (l1 <<< l2)
instance composeLensesCPrismCTraversal ::
  ComposeLenses CPrism CTraversal CTraversal where
  composeLenses (CPrism l1) (CTraversal l2) = CTraversal (l1 <<< l2)

-- Traversal <<< ?
instance composeLensesCTraversalCOptic ::
  ComposeLenses CTraversal COptic CTraversal where
  composeLenses (CTraversal l1) (COptic l2) = CTraversal (l1 <<< l2)
instance composeLensesCTraversalCIso ::
  ComposeLenses CTraversal CIso CTraversal where
  composeLenses (CTraversal l1) (CIso l2) = CTraversal (l1 <<< l2)
instance composeLensesCTraversalCLens ::
  ComposeLenses CTraversal CLens CTraversal where
  composeLenses (CTraversal l1) (CLens l2) = CTraversal (l1 <<< l2)
instance composeLensesCTraversalCPrism ::
  ComposeLenses CTraversal CPrism CTraversal where
  composeLenses (CTraversal l1) (CPrism l2) = CTraversal (l1 <<< l2)
instance composeLensesCTraversalCTraversal ::
  ComposeLenses CTraversal CTraversal CTraversal where
  composeLenses (CTraversal l1) (CTraversal l2) = CTraversal (l1 <<< l2)
