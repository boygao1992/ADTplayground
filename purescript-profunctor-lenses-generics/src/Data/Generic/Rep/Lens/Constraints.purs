module Data.Generic.Rep.Lens.Constraints where

import Prelude

import Data.Maybe
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

----------
-- CLenses

class CLenses (p :: Type -> Type -> Type -> Type -> Type)
instance cLensesCOptic :: CLenses COptic
instance cLensesCIso :: CLenses CIso
instance cLensesCLens :: CLenses CLens
instance cLensesCPrism :: CLenses CPrism
instance cLensesCTraversal :: CLenses CTraversal

---------------------
-- CLenses > CPreview
class CLenses l <= CPreview l where
  cPreview :: forall s t a b. l s t a b -> s -> Maybe a

cPreviewOn :: forall l s t a b. CPreview l => s -> l s t a b -> Maybe a
cPreviewOn s l = cPreview l s
infixl 8 cPreviewOn as .^?

instance cPreviewCOptic :: CPreview COptic where
  cPreview (COptic l) = preview l
instance cPreviewCIso :: CPreview CIso where
  cPreview (CIso l) = preview l
instance cPreviewCLens :: CPreview CLens where
  cPreview (CLens l) = preview l
instance cPreviewCPrism :: CPreview CPrism where
  cPreview (CPrism l) = preview l
instance cPreviewCTraversal :: CPreview CTraversal where
  cPreview (CTraversal l) = preview l

-----------------
-- CLenses > CSet
class CLenses l <= CSet l where
  cSet :: forall s t a b. l s t a b -> b -> s -> t
infixr 4 cSet as ..~

instance cSetCOptic :: CSet COptic where
  cSet (COptic l) = set l
instance cSetCIso :: CSet CIso where
  cSet (CIso l) = set l
instance cSetCLens :: CSet CLens where
  cSet (CLens l) = set l
instance cSetCPrism :: CSet CPrism where
  cSet (CPrism l) = set l
instance cSetCTraversal :: CSet CTraversal where
  cSet (CTraversal l) = set l

---------------------
-- CLenses > CCompose

class (CLenses l1, CLenses l2, CLenses l) <=
  CCompose l1 l2 l | l1 l2 -> l
  where
    cCompose :: forall u v s t a b. l1 u v s t -> l2 s t a b -> l u v a b
infixr 7 cCompose as .<<<

-- Optic <<< ?
instance cComposeCOpticCOptic ::
  CCompose COptic COptic COptic where
  cCompose (COptic l1) (COptic l2) = COptic (l1 <<< l2)
instance cComposeCOpticCIso ::
  CCompose COptic CIso CIso where
  cCompose (COptic l1) (CIso l2) = CIso (l1 <<< l2)
instance cComposeCOpticCLens ::
  CCompose COptic CLens CLens where
  cCompose (COptic l1) (CLens l2) = CLens (l1 <<< l2)
instance cComposeCOpticCPrism ::
  CCompose COptic CPrism CPrism where
  cCompose (COptic l1) (CPrism l2) = CPrism (l1 <<< l2)
instance cComposeCOpticCTraversal ::
  CCompose COptic CTraversal CTraversal where
  cCompose (COptic l1) (CTraversal l2) = CTraversal (l1 <<< l2)
-- Iso <<< ?
instance cComposeCIsoCOptic ::
  CCompose CIso COptic CIso where
  cCompose (CIso l1) (COptic l2) = CIso (l1 <<< l2)
instance cComposeCIsoCIso ::
  CCompose CIso CIso CIso where
  cCompose (CIso l1) (CIso l2) = CIso (l1 <<< l2)
instance cComposeCIsoCLens ::
  CCompose CIso CLens CLens where
  cCompose (CIso l1) (CLens l2) = CLens (l1 <<< l2)
instance cComposeCIsoCPrism ::
  CCompose CIso CPrism CPrism where
  cCompose (CIso l1) (CPrism l2) = CPrism (l1 <<< l2)
instance cComposeCIsoCTraversal ::
  CCompose CIso CTraversal CTraversal where
  cCompose (CIso l1) (CTraversal l2) = CTraversal (l1 <<< l2)
-- Lens <<< ?
instance cComposeCLensCOptic ::
  CCompose CLens COptic CLens where
  cCompose (CLens l1) (COptic l2) = CLens (l1 <<< l2)
instance cComposeCLensCIso ::
  CCompose CLens CIso CLens where
  cCompose (CLens l1) (CIso l2) = CLens (l1 <<< l2)
instance cComposeCLensCLens ::
  CCompose CLens CLens CLens where
  cCompose (CLens l1) (CLens l2) = CLens (l1 <<< l2)
instance cComposeCLensCPrism ::
  CCompose CLens CPrism CTraversal where
  cCompose (CLens l1) (CPrism l2) = CTraversal (l1 <<< l2)
instance cComposeCLensCTraversal ::
  CCompose CLens CTraversal CTraversal where
  cCompose (CLens l1) (CTraversal l2) = CTraversal (l1 <<< l2)
-- Prism <<< ?
instance cComposeCPrismCOptic ::
  CCompose CPrism COptic CPrism where
  cCompose (CPrism l1) (COptic l2) = CPrism (l1 <<< l2)
instance cComposeCPrismCIso ::
  CCompose CPrism CIso CPrism where
  cCompose (CPrism l1) (CIso l2) = CPrism (l1 <<< l2)
instance cComposeCPrismCLens ::
  CCompose CPrism CLens CTraversal where
  cCompose (CPrism l1) (CLens l2) = CTraversal (l1 <<< l2)
instance cComposeCPrismCPrism ::
  CCompose CPrism CPrism CPrism where
  cCompose (CPrism l1) (CPrism l2) = CPrism (l1 <<< l2)
instance cComposeCPrismCTraversal ::
  CCompose CPrism CTraversal CTraversal where
  cCompose (CPrism l1) (CTraversal l2) = CTraversal (l1 <<< l2)
-- Traversal <<< ?
instance cComposeCTraversalCOptic ::
  CCompose CTraversal COptic CTraversal where
  cCompose (CTraversal l1) (COptic l2) = CTraversal (l1 <<< l2)
instance cComposeCTraversalCIso ::
  CCompose CTraversal CIso CTraversal where
  cCompose (CTraversal l1) (CIso l2) = CTraversal (l1 <<< l2)
instance cComposeCTraversalCLens ::
  CCompose CTraversal CLens CTraversal where
  cCompose (CTraversal l1) (CLens l2) = CTraversal (l1 <<< l2)
instance cComposeCTraversalCPrism ::
  CCompose CTraversal CPrism CTraversal where
  cCompose (CTraversal l1) (CPrism l2) = CTraversal (l1 <<< l2)
instance cComposeCTraversalCTraversal ::
  CCompose CTraversal CTraversal CTraversal where
  cCompose (CTraversal l1) (CTraversal l2) = CTraversal (l1 <<< l2)
