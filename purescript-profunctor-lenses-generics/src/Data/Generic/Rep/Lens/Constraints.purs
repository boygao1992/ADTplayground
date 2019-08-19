module Data.Generic.Rep.Lens.Constraints where

import Prelude

import Data.Lens

newtype COptic s t a b = COptic (forall p. Optic p s t a b)
newtype COptic' s a = COptic' (forall p. Optic' p s a)
newtype CIso s t a b = CIso (Iso s t a b)
newtype CIso' s a = CIso' (Iso' s a)
newtype CLens s t a b = CLens (Lens s t a b)
newtype CLens' s a = CLens' (Lens' s a)
newtype CPrism s t a b = CPrism (Prism s t a b)
newtype CPrism' s a = CPrism' (Prism' s a)
-- TODO AffineTraversal
newtype CTraversal s t a b = CTraversal (Traversal s t a b)
newtype CTraversal' s a = CTraversal' (Traversal' s a)

unwrapOptic :: forall s t a b. COptic s t a b -> (forall p. Optic p s t a b)
unwrapOptic (COptic l) = l
unwrapOptic' :: forall s a. COptic' s a -> (forall p. Optic' p s a)
unwrapOptic' (COptic' l) = l
unwrapIso :: forall s t a b. CIso s t a b -> Iso s t a b
unwrapIso (CIso l) = l
unwrapIso' :: forall s a. CIso' s a -> Iso' s a
unwrapIso' (CIso' l) = l
unwrapLens :: forall s t a b. CLens s t a b -> Lens s t a b
unwrapLens (CLens l) = l
unwrapLens' :: forall s a. CLens' s a -> Lens' s a
unwrapLens' (CLens' l) = l
unwrapPrism :: forall s t a b. CPrism s t a b -> Prism s t a b
unwrapPrism (CPrism l) = l
unwrapPrism' :: forall s a . CPrism' s a -> Prism' s a
unwrapPrism' (CPrism' l) = l
unwrapTraversal :: forall s t a b. CTraversal s t a b -> Traversal s t a b
unwrapTraversal (CTraversal l) = l
unwrapTraversal' :: forall s a . CTraversal' s a -> Traversal' s a
unwrapTraversal' (CTraversal' l) = l

-- foreign import kind Constraint
-- foreign import data CProfunctor :: Constraint
-- foreign import data CStrong :: Constraint
-- foreign import data CChoice :: Constraint
-- foreign import data CWander :: Constraint

class ComposeLenses lens1 lens2 o | lens1 lens2 -> o where
  composeLenses :: lens1 -> lens2 -> o

-- Optic <<< ?
instance composeLensesCOpticCOptic ::
  ComposeLenses (COptic u v s t) (COptic s t a b) (COptic u v a b) where
  composeLenses (COptic l1) (COptic l2) = COptic (l1 <<< l2)
instance composeLensesCOpticCIso ::
  ComposeLenses (COptic u v s t) (CIso s t a b) (CIso u v a b) where
  composeLenses (COptic l1) (CIso l2) = CIso (l1 <<< l2)
instance composeLensesCOpticCLens ::
  ComposeLenses (COptic u v s t) (CLens s t a b) (CLens u v a b) where
  composeLenses (COptic l1) (CLens l2) = CLens (l1 <<< l2)
instance composeLensesCOpticCPrism ::
  ComposeLenses (COptic u v s t) (CPrism s t a b) (CPrism u v a b) where
  composeLenses (COptic l1) (CPrism l2) = CPrism (l1 <<< l2)
instance composeLensesCOpticCTraversal ::
  ComposeLenses (COptic u v s t) (CTraversal s t a b) (CTraversal u v a b) where
  composeLenses (COptic l1) (CTraversal l2) = CTraversal (l1 <<< l2)

-- Iso <<< ?
instance composeLensesCIsoCOptic ::
  ComposeLenses (CIso u v s t) (COptic s t a b) (CIso u v a b) where
  composeLenses (CIso l1) (COptic l2) = CIso (l1 <<< l2)
instance composeLensesCIsoCIso ::
  ComposeLenses (CIso u v s t) (CIso s t a b) (CIso u v a b) where
  composeLenses (CIso l1) (CIso l2) = CIso (l1 <<< l2)
instance composeLensesCIsoCLens ::
  ComposeLenses (CIso u v s t) (CLens s t a b) (CLens u v a b) where
  composeLenses (CIso l1) (CLens l2) = CLens (l1 <<< l2)
instance composeLensesCIsoCPrism ::
  ComposeLenses (CIso u v s t) (CPrism s t a b) (CPrism u v a b) where
  composeLenses (CIso l1) (CPrism l2) = CPrism (l1 <<< l2)
instance composeLensesCIsoCTraversal ::
  ComposeLenses (CIso u v s t) (CTraversal s t a b) (CTraversal u v a b) where
  composeLenses (CIso l1) (CTraversal l2) = CTraversal (l1 <<< l2)

-- Lens <<< ?
instance composeLensesCLensCOptic ::
  ComposeLenses (CLens u v s t) (COptic s t a b) (CLens u v a b) where
  composeLenses (CLens l1) (COptic l2) = CLens (l1 <<< l2)
instance composeLensesCLensCIso ::
  ComposeLenses (CLens u v s t) (CIso s t a b) (CLens u v a b) where
  composeLenses (CLens l1) (CIso l2) = CLens (l1 <<< l2)
instance composeLensesCLensCLens ::
  ComposeLenses (CLens u v s t) (CLens s t a b) (CLens u v a b) where
  composeLenses (CLens l1) (CLens l2) = CLens (l1 <<< l2)
instance composeLensesCLensCPrism ::
  ComposeLenses (CLens u v s t) (CPrism s t a b) (CTraversal u v a b) where
  composeLenses (CLens l1) (CPrism l2) = CTraversal (l1 <<< l2)
instance composeLensesCLensCTraversal ::
  ComposeLenses (CLens u v s t) (CTraversal s t a b) (CTraversal u v a b) where
  composeLenses (CLens l1) (CTraversal l2) = CTraversal (l1 <<< l2)

-- Prism <<< ?
instance composeLensesCPrismCOptic ::
  ComposeLenses (CPrism u v s t) (COptic s t a b) (CPrism u v a b) where
  composeLenses (CPrism l1) (COptic l2) = CPrism (l1 <<< l2)
instance composeLensesCPrismCIso ::
  ComposeLenses (CPrism u v s t) (CIso s t a b) (CPrism u v a b) where
  composeLenses (CPrism l1) (CIso l2) = CPrism (l1 <<< l2)
instance composeLensesCPrismCLens ::
  ComposeLenses (CPrism u v s t) (CLens s t a b) (CTraversal u v a b) where
  composeLenses (CPrism l1) (CLens l2) = CTraversal (l1 <<< l2)
instance composeLensesCPrismCPrism ::
  ComposeLenses (CPrism u v s t) (CPrism s t a b) (CPrism u v a b) where
  composeLenses (CPrism l1) (CPrism l2) = CPrism (l1 <<< l2)
instance composeLensesCPrismCTraversal ::
  ComposeLenses (CPrism u v s t) (CTraversal s t a b) (CTraversal u v a b) where
  composeLenses (CPrism l1) (CTraversal l2) = CTraversal (l1 <<< l2)

-- Traversal <<< ?
instance composeLensesCTraversalCOptic ::
  ComposeLenses (CTraversal u v s t) (COptic s t a b) (CTraversal u v a b) where
  composeLenses (CTraversal l1) (COptic l2) = CTraversal (l1 <<< l2)
instance composeLensesCTraversalCIso ::
  ComposeLenses (CTraversal u v s t) (CIso s t a b) (CTraversal u v a b) where
  composeLenses (CTraversal l1) (CIso l2) = CTraversal (l1 <<< l2)
instance composeLensesCTraversalCLens ::
  ComposeLenses (CTraversal u v s t) (CLens s t a b) (CTraversal u v a b) where
  composeLenses (CTraversal l1) (CLens l2) = CTraversal (l1 <<< l2)
instance composeLensesCTraversalCPrism ::
  ComposeLenses (CTraversal u v s t) (CPrism s t a b) (CTraversal u v a b) where
  composeLenses (CTraversal l1) (CPrism l2) = CTraversal (l1 <<< l2)
instance composeLensesCTraversalCTraversal ::
  ComposeLenses (CTraversal u v s t) (CTraversal s t a b) (CTraversal u v a b) where
  composeLenses (CTraversal l1) (CTraversal l2) = CTraversal (l1 <<< l2)
