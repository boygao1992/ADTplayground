module Selda.Table
( module Selda.Table.Type
) where

import Prelude
import Type.Prelude

import Type.Row.Homogeneous

import Data.Maybe
import Data.Exists1
import Data.Exists2
import Data.Tuple.Nested
import Data.Leibniz
import Heterogeneous.Folding
import Foreign.Object (Object)
import Foreign.Object as Object

import Selda.Table.Type (AutoIncType(..), ColAttr(..), ColInfo(..), IndexMethod(..), Table(..), isAutoPrimary, isPrimary, isUnique)
import Selda.Types
import Selda.Selectors

import Unsafe.Coerce

{- NOTE Group
data Group t a where
  (:+)   :: Selector t a -> Group t b -> Group t (a :*: b)
  Single :: Selector t a -> Group t a
infixr 1 :+

-- NOTE first attempt
data Group t a
  = Cons (Exists2 (ConsF2 t a))
  | Single (Selector t a)
data ConsF2 t ab a b = ConsF2 ab (ab ~ (a :+: b))
newtype GroupCons a b = GroupCons (Exists (GroupConsF a b))
data GroupConsF a b t = GroupConsF (Selector t a) (Group t b)
infixr 1 type GroupCons as :+:
groupCons :: forall t a b. Selector t a -> Group t b -> a :+: b
groupCons s g = GroupCons $ mkExists $ GroupConsF s g
infixr 1 groupCons as :+:
consF2 :: forall t a b. Selector t a -> Group t b -> ConsF2 t (a :+: b) a b
consF2 s g = ConsF2 (s :+: g) identity
cons :: forall t a b. Selector t a -> Group t b -> Group t (a :+: b)
cons s g = Cons $ mkExists2 $ consF2 s g
infixr 1 cons as :+
-}

{- NOTE Attr
data Attr a where
  (:-) :: SelectorLike g => g t a -> Attribute g t a -> Attr t
infixl 0 :-

-- NOTE first attempt
data Attr t
  = AttrCons (AttrConsExists t)
newtype AttrConsF t g a = AttrConsF (SelectorLike g => (g t a) /\ (Attribute g t a))
foreign import data AttrConsExists :: Type -> Type
mkAttrConsExists :: forall t g a. AttrConsF t g a -> AttrConsExists t
mkAttrConsExists = unsafeCoerce
runAttrConsExists :: forall t r. (forall g a. AttrConsF t g a -> r) -> AttrConsExists t -> r
runAttrConsExists = unsafeCoerce
attrCons :: forall g t a. SelectorLike g => g t a -> Attribute g t a -> Attr t
attrCons g a = AttrCons $ mkAttrConsExists $ AttrConsF (g /\ a)
infixl 0 attrCons as :-

-- NOTE second attempt
data Attr t
newtype AttrF2 t g a = AttrF2 (g t a /\ Attribute g t a)
mkAttr :: forall t g a. SelectorLike g => g t a -> Attribute g t a -> Attr t
mkAttr g a = unsafeCoerce $ AttrF2 $ g /\ a
runAttr :: forall t r. (forall g a. SelectorLike g => g t a /\ Attribute g t a -> r) -> Attr t -> r
runAttr = unsafeCoerce

-}

{- NOTE Attr t = exists g c. Attribute g t c
old: [Attr t]
new: Homogenous r (Attr t) => { | r}
-}

{-
combinedAttrs =
  [ (ixs, a)
  | sel :- Attribute [a] <- attrs
  , let ixs = indices sel
  , case ixs of
      (_:_:_)              -> True -- TODO Group :- Attribute
      [_] | a == Unique    -> True
      [_] | Indexed _ <- a -> True
      _                    -> False
  ]
-}
data CombinedAttrs
instance foldingWithIndexCombinedAttrs ::
  IsSymbol label
  => FoldingWithIndex CombinedAttrs
    (SProxy label)
    (Object (Array ColAttr))
    (Attribute g t c)
    (Object (Array ColAttr)) where
  foldingWithIndex _ label acc x = case x of
    Attribute colAttrs
      -- TODO
      | [Unique] <- colAttrs
      , [Indexed _] <- colAttrs ->
      Object.update (Just <<< (_ <> colAttrs)) (reflectSymbol label) acc
    _ -> acc

{-
pkAttrs = concat
  [ [(ixs, Primary), (ixs, Required)]
  | sel :- Attribute [Primary,Required] <- attrs
  , let ixs = indices sel
  ]
-}
data PkAttrs
instance foldingWithIndexPkAttrs ::
  IsSymbol label
  => FoldingWithIndex PkAttrs
  (SProxy label)
  (Object (Array ColAttr))
  (Attribute g t c)
  (Object (Array ColAttr)) where
  foldingWithIndex _ label acc x = case x of
    Attribute [Primary, Required] ->
      Object.update (Just <<< (_ <> [Primary, Required])) (reflectSymbol label) acc
    _ -> acc


data Attribute (g :: Type -> Type -> Type) t c
  = Attribute (Array ColAttr)
  | ForeignKey (Table Unit /\ ColName)

class SelectorLike (g :: Type -> Type -> Type) where
  indices :: forall t a. g t a -> Array Int
