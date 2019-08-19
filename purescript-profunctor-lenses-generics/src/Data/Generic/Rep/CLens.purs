module Data.Generic.Rep.CLens where

import Prelude
import Type.Prelude

import Num.Nat (class Succ)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol

import Data.Either
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, to)
import Data.Generic.Rep.Lens.Constraints
import Data.Lens
import Data.Maybe
import Data.Tuple
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Unsafe.Coerce

-----------------------------------
-- Primitive Lenses for Generic Rep

-- | Iso between types and their generic rep.
_Generic
  :: forall s t sRep tRep
  . Generic s sRep
  => Generic t tRep
  => CIso s t sRep tRep
_Generic = CIso (iso from to)

-- | Iso between a type and its rep.
_Generic' :: forall s sRep. Generic s sRep => CIso s s sRep sRep
_Generic' = _Generic

-- | NoConstructors is equivalent to Void
_NoConstructors :: CIso NoConstructors NoConstructors Void Void
_NoConstructors = CIso (iso unsafeCoerce absurd)

-- | Iso between a Constructor and its argument rep
_Constructor
  :: forall label1 label2 arg1 arg2
  . CIso (Constructor label1 arg1) (Constructor label2 arg2) arg1 arg2
_Constructor = CIso (iso (\(Constructor a) -> a) Constructor)

-- | Iso between Sum and Either
_Sum :: forall a b c d. CIso (Sum a b) (Sum c d) (Either a b) (Either c d)
_Sum = CIso
  (iso
    (case _ of
      Inl a -> Left a
      Inr b -> Right b)
    (case _ of
      Left c -> Inl c
      Right d -> Inr d))

-- | Prism into the Inl of Sum
_SumInl :: forall a b r. CPrism (Sum a r) (Sum b r) a b
_SumInl = _Sum .<<< (CPrism _Left)

-- | Prism into the Inr of Sum
_SumInr :: forall l a b. CPrism (Sum l a) (Sum l b) a b
_SumInr = _Sum .<<< (CPrism _Right)

-- | Iso between NoArguments and Unit
_NoArguments :: CIso NoArguments NoArguments Unit Unit
_NoArguments = CIso (iso (const unit) (const NoArguments))

-- | Iso between Argument and its wrapped type
_Argument :: forall a b. CIso (Argument a) (Argument b) a b
_Argument = CIso (iso (\(Argument a) -> a) Argument)

-- | Iso between Product and Tuple
_Product :: forall a b c d. CIso (Product a b) (Product c d) (Tuple a b) (Tuple c d)
_Product = CIso
  (iso
    (\(Product a b) -> Tuple a b)
    (\(Tuple c d) -> Product c d))

-- | Lens into the first of a Product
_ProductFirst :: forall a b r. CLens (Product a r) (Product b r) a b
_ProductFirst = _Product .<<< (CLens _1)

-- | Lens into the second of a Product
_ProductSecond :: forall l a b. CLens (Product l a) (Product l b) a b
_ProductSecond = _Product .<<< (CLens _2)

-------
-- Ctor

class GenericCtor ctor
  (li :: Type -> Type -> Type -> Type -> Type) s i
  (lo :: Type -> Type -> Type -> Type -> Type) a
  | ctor li s i -> lo a
  where
    _GenericCtor :: SProxy ctor -> li s s i i -> lo s s a a

-- Ctor > Arg
class GenericCtorArg
  (li :: Type -> Type -> Type -> Type -> Type) s arg
  (lo :: Type -> Type -> Type -> Type -> Type) a
  | li s arg -> lo a
  where
    _GenericCtorArg :: li s s arg arg -> lo s s a a

-- Ctor > Arg > Product
class GenericCtorArgProduct
  (li :: Type -> Type -> Type -> Type -> Type) s arg
  (lo :: Type -> Type -> Type -> Type -> Type) (a :: # Type)
  | li s arg -> lo a
  where
    _GenericCtorArgProduct :: li s s arg arg -> lo s s (Record a) (Record a)

-- Ctor > Arg > Product > Type
class GenericCtorArgProductType i (no :: Symbol) (from :: # Type) (to :: # Type)
  | i from -> to

instance _GenericCtorArgProductTypeArg ::
  ( Symbol.Append "_" no label
  , Row.Cons label a from to
  )
  => GenericCtorArgProductType (Argument a) no from to

instance _GenericCtorArgProductTypeProduct ::
  ( Succ noL noR
  , GenericCtorArgProductType r noR from to1
  , GenericCtorArgProductType l noL to1 to
  )
  => GenericCtorArgProductType (Product l r) noL from to

-- Ctor > Arg > Product > Lenses
class GenericCtorArgProductLenses
  (li :: Type -> Type -> Type -> Type -> Type) s i (no :: Symbol)
  (from :: # Type) (to :: # Type)
  | li s i no from -> to
  where
    _GenericCtorArgProductLenses
      :: SProxy no -> li s s i i -> Builder (Record from) (Record to)

-- Ctor > Arg > Product > Get
class GenericCtorArgProductGet (lenses :: # Type) s (a :: # Type) where
  _GenericCtorArgProductGet :: Record lenses -> s -> Maybe (Record a)

class GenericCtorArgProductGetRL
  (aRl :: RowList) (lenses :: # Type) s
  (from :: # Type) (to :: # Type)
  | aRl lenses s from -> to
  where
    _GenericCtorArgProductGetRL
      :: RLProxy aRl -> Record lenses -> s -> Maybe (Builder (Record from) (Record to))

-- Ctor > Arg > Product > Set
class GenericCtorArgProductSet (lenses :: # Type) s (a :: # Type) where
  _GenericCtorArgProductSet :: Record lenses -> s -> Record a -> s

class GenericCtorArgProductSetRL (aRl :: RowList) (lenses :: # Type) s (a :: # Type)
  where
    _GenericCtorArgProductSetRL :: RLProxy aRl -> Record lenses -> s -> Record a -> s
