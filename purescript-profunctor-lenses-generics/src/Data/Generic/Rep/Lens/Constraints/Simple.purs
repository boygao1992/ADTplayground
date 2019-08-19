module Data.Generic.Rep.Lens.Constraints.Simple where

import Prelude

import Data.Maybe (Maybe)
import Data.Lens (Iso', Lens', Optic', Prism', Traversal', preview, set)

newtype COptic' s a = COptic' (forall p. Optic' p s a)
newtype CIso' s a = CIso' (Iso' s a)
newtype CLens' s a = CLens' (Lens' s a)
newtype CPrism' s a = CPrism' (Prism' s a)
-- TODO AffineTraversal'
newtype CTraversal' s a = CTraversal' (Traversal' s a)

unwrapOptic' :: forall s a. COptic' s a -> (forall p. Optic' p s a)
unwrapOptic' (COptic' l) = l
unwrapIso' :: forall s a. CIso' s a -> Iso' s a
unwrapIso' (CIso' l) = l
unwrapLens' :: forall s a. CLens' s a -> Lens' s a
unwrapLens' (CLens' l) = l
unwrapPrism' :: forall s a. CPrism' s a -> Prism' s a
unwrapPrism' (CPrism' l) = l
unwrapTraversal' :: forall s a. CTraversal' s a -> Traversal' s a
unwrapTraversal' (CTraversal' l) = l

----------
-- CLenses'

class CLenses' (p :: Type -> Type -> Type)
instance cLenses'COptic'     :: CLenses' COptic'
instance cLenses'CIso'       :: CLenses' CIso'
instance cLenses'CLens'      :: CLenses' CLens'
instance cLenses'CPrism'     :: CLenses' CPrism'
instance cLenses'CTraversal' :: CLenses' CTraversal'

---------------------
-- CLenses' > CPreview'
class CLenses' l <= CPreview' l where
  cPreview' :: forall s a. l s a -> s -> Maybe a

cPreview'On :: forall l s a. CPreview' l => s -> l s a -> Maybe a
cPreview'On s l = cPreview' l s
infixl 8 cPreview'On as .^?

instance cPreview'COptic' :: CPreview' COptic' where
  cPreview' (COptic' l) = preview l
instance cPreview'CIso' :: CPreview' CIso' where
  cPreview' (CIso' l) = preview l
instance cPreview'CLens' :: CPreview' CLens' where
  cPreview' (CLens' l) = preview l
instance cPreview'CPrism' :: CPreview' CPrism' where
  cPreview' (CPrism' l) = preview l
instance cPreview'CTraversal' :: CPreview' CTraversal' where
  cPreview' (CTraversal' l) = preview l

-----------------
-- CLenses' > CSet'
class CLenses' l <= CSet' l where
  cSet' :: forall s a. l s a -> a -> s -> s
infixr 4 cSet' as ..~

instance cSet'COptic' :: CSet' COptic' where
  cSet' (COptic' l) = set l
instance cSet'CIso' :: CSet' CIso' where
  cSet' (CIso' l) = set l
instance cSet'CLens' :: CSet' CLens' where
  cSet' (CLens' l) = set l
instance cSet'CPrism' :: CSet' CPrism' where
  cSet' (CPrism' l) = set l
instance cSet'CTraversal' :: CSet' CTraversal' where
  cSet' (CTraversal' l) = set l

---------------------
-- CLenses' > CCompose'

class (CLenses' l1, CLenses' l2, CLenses' l) <=
  CCompose' l1 l2 l | l1 l2 -> l
  where
    cCompose' :: forall a b c. l1 a b -> l2 b c -> l a c
infixr 7 cCompose' as .<<<

-- Optic' <<< ?
instance cCompose'COptic'COptic' ::
  CCompose' COptic' COptic' COptic' where
  cCompose' (COptic' l1) (COptic' l2) = COptic' (l1 <<< l2)
instance cCompose'COptic'CIso' ::
  CCompose' COptic' CIso' CIso' where
  cCompose' (COptic' l1) (CIso' l2) = CIso' (l1 <<< l2)
instance cCompose'COptic'CLens' ::
  CCompose' COptic' CLens' CLens' where
  cCompose' (COptic' l1) (CLens' l2) = CLens' (l1 <<< l2)
instance cCompose'COptic'CPrism' ::
  CCompose' COptic' CPrism' CPrism' where
  cCompose' (COptic' l1) (CPrism' l2) = CPrism' (l1 <<< l2)
instance cCompose'COptic'CTraversal' ::
  CCompose' COptic' CTraversal' CTraversal' where
  cCompose' (COptic' l1) (CTraversal' l2) = CTraversal' (l1 <<< l2)
-- Iso' <<< ?
instance cCompose'CIso'COptic' ::
  CCompose' CIso' COptic' CIso' where
  cCompose' (CIso' l1) (COptic' l2) = CIso' (l1 <<< l2)
instance cCompose'CIso'CIso' ::
  CCompose' CIso' CIso' CIso' where
  cCompose' (CIso' l1) (CIso' l2) = CIso' (l1 <<< l2)
instance cCompose'CIso'CLens' ::
  CCompose' CIso' CLens' CLens' where
  cCompose' (CIso' l1) (CLens' l2) = CLens' (l1 <<< l2)
instance cCompose'CIso'CPrism' ::
  CCompose' CIso' CPrism' CPrism' where
  cCompose' (CIso' l1) (CPrism' l2) = CPrism' (l1 <<< l2)
instance cCompose'CIso'CTraversal' ::
  CCompose' CIso' CTraversal' CTraversal' where
  cCompose' (CIso' l1) (CTraversal' l2) = CTraversal' (l1 <<< l2)
-- Lens' <<< ?
instance cCompose'CLens'COptic' ::
  CCompose' CLens' COptic' CLens' where
  cCompose' (CLens' l1) (COptic' l2) = CLens' (l1 <<< l2)
instance cCompose'CLens'CIso' ::
  CCompose' CLens' CIso' CLens' where
  cCompose' (CLens' l1) (CIso' l2) = CLens' (l1 <<< l2)
instance cCompose'CLens'CLens' ::
  CCompose' CLens' CLens' CLens' where
  cCompose' (CLens' l1) (CLens' l2) = CLens' (l1 <<< l2)
instance cCompose'CLens'CPrism' ::
  CCompose' CLens' CPrism' CTraversal' where
  cCompose' (CLens' l1) (CPrism' l2) = CTraversal' (l1 <<< l2)
instance cCompose'CLens'CTraversal' ::
  CCompose' CLens' CTraversal' CTraversal' where
  cCompose' (CLens' l1) (CTraversal' l2) = CTraversal' (l1 <<< l2)
-- Prism' <<< ?
instance cCompose'CPrism'COptic' ::
  CCompose' CPrism' COptic' CPrism' where
  cCompose' (CPrism' l1) (COptic' l2) = CPrism' (l1 <<< l2)
instance cCompose'CPrism'CIso' ::
  CCompose' CPrism' CIso' CPrism' where
  cCompose' (CPrism' l1) (CIso' l2) = CPrism' (l1 <<< l2)
instance cCompose'CPrism'CLens' ::
  CCompose' CPrism' CLens' CTraversal' where
  cCompose' (CPrism' l1) (CLens' l2) = CTraversal' (l1 <<< l2)
instance cCompose'CPrism'CPrism' ::
  CCompose' CPrism' CPrism' CPrism' where
  cCompose' (CPrism' l1) (CPrism' l2) = CPrism' (l1 <<< l2)
instance cCompose'CPrism'CTraversal' ::
  CCompose' CPrism' CTraversal' CTraversal' where
  cCompose' (CPrism' l1) (CTraversal' l2) = CTraversal' (l1 <<< l2)
-- Traversal' <<< ?
instance cCompose'CTraversal'COptic' ::
  CCompose' CTraversal' COptic' CTraversal' where
  cCompose' (CTraversal' l1) (COptic' l2) = CTraversal' (l1 <<< l2)
instance cCompose'CTraversal'CIso' ::
  CCompose' CTraversal' CIso' CTraversal' where
  cCompose' (CTraversal' l1) (CIso' l2) = CTraversal' (l1 <<< l2)
instance cCompose'CTraversal'CLens' ::
  CCompose' CTraversal' CLens' CTraversal' where
  cCompose' (CTraversal' l1) (CLens' l2) = CTraversal' (l1 <<< l2)
instance cCompose'CTraversal'CPrism' ::
  CCompose' CTraversal' CPrism' CTraversal' where
  cCompose' (CTraversal' l1) (CPrism' l2) = CTraversal' (l1 <<< l2)
instance cCompose'CTraversal'CTraversal' ::
  CCompose' CTraversal' CTraversal' CTraversal' where
  cCompose' (CTraversal' l1) (CTraversal' l2) = CTraversal' (l1 <<< l2)
