module Data.Generic.Rep.CLens where

import Prelude
import Type.Prelude

import Num.Nat (class Succ)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Beside, Text)

import Control.Apply (lift2)
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

infixr 2 type Beside as <>


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
_SumInl = _Sum .<<< CPrism _Left

-- | Prism into the Inr of Sum
_SumInr :: forall l a b. CPrism (Sum l a) (Sum l b) a b
_SumInr = _Sum .<<< CPrism _Right

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
_ProductFirst = _Product .<<< CLens _1

-- | Lens into the second of a Product
_ProductSecond :: forall l a b. CLens (Product l a) (Product l b) a b
_ProductSecond = _Product .<<< CLens _2

-------
-- Ctor

_Ctor'
  :: forall ctor s a rep lo
  . Generic s rep
  => GenericCtor ctor CIso s rep lo a
  => Proxy s
  -> SProxy ctor
  -> lo s s a a
_Ctor' _ ctor = _GenericCtor ctor _Generic'

class GenericCtor ctor
  (li :: Type -> Type -> Type -> Type -> Type) s i
  (lo :: Type -> Type -> Type -> Type -> Type) a
  | ctor li s i -> lo a
  where
    _GenericCtor :: SProxy ctor -> li s s i i -> lo s s a a

instance genericCtorSumFound ::
  ( CCompose li CPrism li1
  , GenericCtorArg li1 s arg lo a
  )
  => GenericCtor ctor li s (Sum (Constructor ctor arg) r) lo a
  where
    _GenericCtor ctor _i = _GenericCtorArg (_i .<<< _SumInl .<<< _Constructor)
else
instance genericCtorSumNext ::
  ( CCompose li CPrism li1
  , GenericCtor ctor li1 s r lo a
  )
  => GenericCtor ctor li s (Sum l r) lo a
  where
    _GenericCtor ctor _i = _GenericCtor ctor (_i .<<< _SumInr)
else
instance genericCtorSumLast ::
  ( CCompose li CIso li1
  , GenericCtorArg li1 s arg lo a
  )
  => GenericCtor ctor li s (Constructor ctor arg) lo a
  where
    _GenericCtor ctor _i = _GenericCtorArg (_i .<<< _Constructor)
else
instance genericCtorSumFail ::
  Fail (
    Text "No constructors found called `" <>
    Text ctor <>
    Text "`" ) =>
  GenericCtor ctor li s (Constructor other b) lo a where
    _GenericCtor _ = unsafeCoerce

-- Ctor > Arg
class GenericCtorArg
  (li :: Type -> Type -> Type -> Type -> Type) s arg
  (lo :: Type -> Type -> Type -> Type -> Type) a
  | li s arg -> lo a
  where
    _GenericCtorArg :: li s s arg arg -> lo s s a a

instance genericCtorArgMatch ::
  CCompose li CIso lo
  => GenericCtorArg li s (Argument a) lo a where
  _GenericCtorArg _i = _i .<<< _Argument

instance genericCtorArgNone ::
  CCompose li CIso lo
  => GenericCtorArg li s NoArguments lo Unit where
  _GenericCtorArg _i = _i .<<< _NoArguments

instance genericCtorArgProduct ::
  GenericCtorArgProduct li s (Product l r) a
  => GenericCtorArg li s (Product l r) CTraversal (Record a)
  where
    _GenericCtorArg = _GenericCtorArgProduct

-- Ctor > Arg > Product
class GenericCtorArgProduct
  (li :: Type -> Type -> Type -> Type -> Type) s arg
  (a :: # Type)
  | li s arg -> a
  where
    _GenericCtorArgProduct :: li s s arg arg -> CTraversal s s (Record a) (Record a)

instance genericCtorArgProductImpl ::
  ( GenericCtorArgProductType "1" (Product l r) () a
  , GenericCtorArgProductLenses "1" li s (Product l r) () lenses
  , GenericCtorArgProductGet lenses s a
  , GenericCtorArgProductSet lenses s a
  )
  => GenericCtorArgProduct li s (Product l r) a
  where
    _GenericCtorArgProduct _i =
      let
        (lenses :: Record lenses)
          = Builder.build <@> {}
            $ _GenericCtorArgProductLenses (SProxy :: SProxy "1") _i

        (get :: s -> Maybe (Record a))
          = _GenericCtorArgProductGet lenses

        (set :: s -> Record a -> s)
          = _GenericCtorArgProductSet lenses

        merge :: forall f. Applicative f => (Record a -> f (Record a)) -> s -> f s
        merge coalg s =
          fromMaybe (pure s)
          $ map (map $ set s)
          $ map coalg
          $ get s
      in
       CTraversal (wander merge)

-- Ctor > Arg > Product > Type
class GenericCtorArgProductType (no :: Symbol) i (from :: # Type) (to :: # Type)
  | no i from -> to

instance _GenericCtorArgProductTypeArg ::
  ( Symbol.Append "_" no label
  , Row.Cons label a from to
  )
  => GenericCtorArgProductType no (Argument a) from to

instance _GenericCtorArgProductTypeProduct ::
  ( Succ noL noR
  , GenericCtorArgProductType noR r from to1
  , GenericCtorArgProductType noL l to1 to
  )
  => GenericCtorArgProductType noL (Product l r) from to

-- Ctor > Arg > Product > Lenses
class GenericCtorArgProductLenses (no :: Symbol) -- TODO swapped
  (li :: Type -> Type -> Type -> Type -> Type) s i
  (from :: # Type) (to :: # Type)
  | no li s i from -> to
  where
    _GenericCtorArgProductLenses
      :: SProxy no -> li s s i i -> Builder (Record from) (Record to)

instance _GenericCtorArgProductArgument ::
  ( Symbol.Append "_" no _label
  , CCompose li CIso lo
  , IsSymbol _label
  , Row.Lacks _label from
  , Row.Cons _label (lo s s a a) from to
  )
  => GenericCtorArgProductLenses no li s (Argument a) from to
  where
    _GenericCtorArgProductLenses _ _i
      = Builder.insert (SProxy :: SProxy _label)
        (_i .<<< _Argument)

instance _GenericCtorArgProductProduct ::
  ( Succ noL noR
  , CCompose li CLens lo
  , GenericCtorArgProductLenses noR lo s r from to1
  , GenericCtorArgProductLenses noL lo s l to1 to
  )
  => GenericCtorArgProductLenses noL li s (Product l r) from to
  where
    _GenericCtorArgProductLenses _ _i
      = _GenericCtorArgProductLenses (SProxy :: SProxy noL) (_i .<<< _ProductFirst)
        <<< _GenericCtorArgProductLenses (SProxy :: SProxy noR)(_i .<<< _ProductSecond)

-- Ctor > Arg > Product > Get
class GenericCtorArgProductGet (lenses :: # Type) s (a :: # Type) where
  _GenericCtorArgProductGet :: Record lenses -> s -> Maybe (Record a)

instance _GenericCtorArgProductGetImpl ::
  ( RowToList a aRl
  , GenericCtorArgProductGetRL aRl lenses s () a
  )
  => GenericCtorArgProductGet lenses s a
  where
    _GenericCtorArgProductGet lenses s
      = map (Builder.build <@> {})
        $ _GenericCtorArgProductGetRL (RLProxy :: RLProxy aRl) lenses s

class GenericCtorArgProductGetRL
  (aRl :: RowList) (lenses :: # Type) s
  (from :: # Type) (to :: # Type)
  | aRl lenses s from -> to
  where
    _GenericCtorArgProductGetRL
      :: RLProxy aRl -> Record lenses -> s -> Maybe (Builder (Record from) (Record to))

instance _GenericCtorArgProductGetRLNil ::
  GenericCtorArgProductGetRL RowList.Nil lenses s from from
  where
    _GenericCtorArgProductGetRL _ lenses s = Just identity

instance _GenericCtorArgProductGetRLCons ::
  ( GenericCtorArgProductGetRL restRl lenses s from to1
  , IsSymbol label
  , Row.Lacks label to1
  , Row.Cons label typ to1 to
  , Row.Cons label (l s s typ typ) restLenses lenses -- DONE
  , CPreview l
  )
  => GenericCtorArgProductGetRL (RowList.Cons label typ restRl) lenses s from to
  where
    _GenericCtorArgProductGetRL _ lenses s
      = lift2 (<<<)
        ( Builder.insert (SProxy :: SProxy label)
          <$> s .^? (Record.get (SProxy :: SProxy label) lenses)
        )
        ( _GenericCtorArgProductGetRL (RLProxy :: RLProxy restRl)
          lenses s
        )

-- Ctor > Arg > Product > Set
class GenericCtorArgProductSet (lenses :: # Type) s (a :: # Type) where
  _GenericCtorArgProductSet :: Record lenses -> s -> Record a -> s

instance _GenericCtorArgProductSetImpl ::
  ( RowToList a aRl
  , GenericCtorArgProductSetRL aRl lenses s a
  )
  => GenericCtorArgProductSet lenses s a
  where
    _GenericCtorArgProductSet lenses s a =
      _GenericCtorArgProductSetRL (RLProxy :: RLProxy aRl) lenses s a

class GenericCtorArgProductSetRL (aRl :: RowList) (lenses :: # Type) s (a :: # Type)
  where
    _GenericCtorArgProductSetRL :: RLProxy aRl -> Record lenses -> s -> Record a -> s

instance _GenericCtorArgProductSetRLNil ::
  GenericCtorArgProductSetRL RowList.Nil lenses s a
  where
    _GenericCtorArgProductSetRL _ _ s _ = s

instance _GenericCtorArgProductSetRLCons ::
  ( GenericCtorArgProductSetRL restRl lenses s a
  , IsSymbol label
  , Row.Cons label (l s s typ typ) restLenses lenses -- DONE
  , Row.Cons label typ restA a
  , CSet l
  )
  => GenericCtorArgProductSetRL (RowList.Cons label typ restRl) lenses s a
  where
    _GenericCtorArgProductSetRL _ lenses s a =
      _GenericCtorArgProductSetRL (RLProxy :: RLProxy restRl)
        lenses
        (s # (Record.get (SProxy :: SProxy label) lenses)
        ..~ (Record.get (SProxy :: SProxy label) a)
        )
        a
