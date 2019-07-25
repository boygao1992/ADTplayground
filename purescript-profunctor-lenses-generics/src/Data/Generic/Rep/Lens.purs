module Data.Generic.Rep.Lens where

import Prelude

import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, to)
import Data.Identity (Identity)
import Data.Lens (class Wander, Iso, Iso', Lens, Optic', Prism, _1, _2, _Left, _Right, iso, wander, (.~), (^?))
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Num.Nat (class Succ)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Beside, Text)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(..), SProxy(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

infixr 2 type Beside as <>

genericLens :: forall p s o. GenericTypeSort p s s o => Proxy s -> o
genericLens s = genericTypeSort s (identity :: Optic' p s s)

-----------------------------------
-- Primitive Lenses for Generic Rep

-- | Iso between types and their generic rep.
_Generic
  :: forall s t sRep tRep
  . Generic s sRep
  => Generic t tRep
  => Iso s t sRep tRep
_Generic = iso from to

-- | Iso between a type and its rep.
_Generic' :: forall s sRep. Generic s sRep => Iso' s sRep
_Generic' = _Generic

-- | NoConstructors is equivalent to Void
_NoConstructors :: Iso' NoConstructors Void
_NoConstructors = iso unsafeCoerce absurd

-- | Iso between a Constructor and its argument rep
_Constructor
  :: forall label1 label2 arg1 arg2
  . Iso (Constructor label1 arg1) (Constructor label2 arg2) arg1 arg2
_Constructor = iso (\(Constructor a) -> a) Constructor

-- | Iso between Sum and Either
_Sum :: forall a b c d. Iso (Sum a b) (Sum c d) (Either a b) (Either c d)
_Sum = iso
  (case _ of
    Inl a -> Left a
    Inr b -> Right b)
  (case _ of
    Left c -> Inl c
    Right d -> Inr d)

-- | Prism into the Inl of Sum
_SumInl :: forall a b r. Prism (Sum a r) (Sum b r) a b
_SumInl = _Sum <<< _Left

-- | Prism into the Inr of Sum
_SumInr :: forall l a b. Prism (Sum l a) (Sum l b) a b
_SumInr = _Sum <<< _Right

-- | Iso between NoArguments and Unit
_NoArguments :: Iso' NoArguments Unit
_NoArguments = iso (const unit) (const NoArguments)

-- | Iso between Argument and its wrapped type
_Argument :: forall a b. Iso (Argument a) (Argument b) a b
_Argument = iso (\(Argument a) -> a) Argument

-- | Iso between Product and Tuple
_Product :: forall a b c d. Iso (Product a b) (Product c d) (Tuple a b) (Tuple c d)
_Product = iso
  (\(Product a b) -> Tuple a b)
  (\(Tuple c d) -> Product c d)

-- | Lens into the first of a Product
_ProductFirst :: forall a b r. Lens (Product a r) (Product b r) a b
_ProductFirst = _Product <<< _1

-- | Lens into the second of a Product
_ProductSecond :: forall l a b. Lens (Product l a) (Product l b) a b
_ProductSecond = _Product <<< _2

-------
-- Ctor

_Ctor'
  :: forall ctor s a p rep
  . Generic s rep
  => GenericCtor p ctor rep rep a
  => Profunctor p
  => SProxy ctor
  -> Optic' p s a
_Ctor' ctor = _Generic' <<< _GenericCtor ctor identity

class GenericCtor p ctor s i a | p ctor s i -> a where
  _GenericCtor :: SProxy ctor -> Optic' p s i -> Optic' p s a

instance genericCtorSumFound
  :: ( Choice p
    , GenericCtorArg p s arg a
    )
  => GenericCtor p ctor s (Sum (Constructor ctor arg) r) a
  where
    _GenericCtor ctor _i = _GenericCtorArg (_i <<< _SumInl <<< _Constructor)
else
instance genericCtorSumNext
  :: ( Choice p
    , GenericCtor p ctor s r a
    )
  => GenericCtor p ctor s (Sum l r) a
  where
    _GenericCtor ctor _i = _GenericCtor ctor (_i <<< _SumInr)
else
instance genericCtorSumLast
  :: ( Profunctor p
    , GenericCtorArg p s arg a
    )
  => GenericCtor p ctor s (Constructor ctor arg) a
  where
    _GenericCtor ctor _i = _GenericCtorArg (_i <<< _Constructor)
else
instance genericCtorSumFail ::
  Fail (
    Text "No constructors found called `" <>
    Text ctor <>
    Text "`" ) =>
  GenericCtor p ctor s (Constructor other b) a where
    _GenericCtor _ _ = unsafeCoerce

-- Ctor > Arg
class GenericCtorArg p s arg a | p s arg -> a where
  _GenericCtorArg :: Optic' p s arg -> Optic' p s a

instance genericCtorArgMatch ::
  Profunctor p =>
  GenericCtorArg p s (Argument a) a where
    _GenericCtorArg _i = _i <<< _Argument

instance genericCtorArgNone ::
  Profunctor p =>
  GenericCtorArg p s NoArguments Unit where
    _GenericCtorArg _i = _i <<< _NoArguments

instance genericCtorArgProduct
  :: GenericCtorArgProduct p s (Product l r) a
  => GenericCtorArg p s (Product l r) (Record a)
  where
    _GenericCtorArg = _GenericCtorArgProduct

-- Ctor > Arg > Product
class GenericCtorArgProduct p s arg (a :: # Type) | p s arg -> a where
  _GenericCtorArgProduct :: Optic' p s arg -> Optic' p s (Record a)

instance genericCtorArgProductImpl
  :: ( GenericCtorArgProductType (Product l r) "1" () a
    , GenericCtorArgProductLenses p s (Product l r) "1" () lenses
    , GenericCtorArgProductGet lenses s a
    , GenericCtorArgProductSet lenses s a
    , Wander p
    )
  => GenericCtorArgProduct p s (Product l r) a
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
       wander merge

-- Ctor > Arg > Product > Type
class GenericCtorArgProductType i (no :: Symbol) (from :: # Type) (to :: # Type)
  | i from -> to

instance _GenericCtorArgProductTypeArg
  :: ( Symbol.Append "_" no label
    , Row.Cons label a from to
    )
  => GenericCtorArgProductType (Argument a) no from to

instance _GenericCtorArgProductTypeProduct
  :: ( Succ noL noR
    , GenericCtorArgProductType r noR from to1
    , GenericCtorArgProductType l noL to1 to
    )
  => GenericCtorArgProductType (Product l r) noL from to

-- Ctor > Arg > Product > Lenses
class GenericCtorArgProductLenses p s i (no :: Symbol)(from :: # Type) (to :: # Type)
  | p s i from -> to
  where
    _GenericCtorArgProductLenses :: SProxy no -> Optic' p s i -> Builder (Record from) (Record to)

instance _GenericCtorArgProductArgument
  :: ( Symbol.Append "_" no _label
    , IsSymbol _label
    , Row.Lacks _label from
    , Row.Cons _label (p a a -> p s s) from to
    , Profunctor p
    )
  => GenericCtorArgProductLenses p s (Argument a) no from to
  where
    _GenericCtorArgProductLenses _ _i
      = Builder.insert (SProxy :: SProxy _label)
          (_i <<< _Argument)

instance _GenericCtorArgProductProduct
  :: ( Succ noL noR
    , GenericCtorArgProductLenses p s r noR from to1
    , GenericCtorArgProductLenses p s l noL to1 to
    , Strong p
    )
  => GenericCtorArgProductLenses p s (Product l r) noL from to
  where
    _GenericCtorArgProductLenses _ _i
      = _GenericCtorArgProductLenses (SProxy :: SProxy noL) (_i <<< _ProductFirst)
      <<< _GenericCtorArgProductLenses (SProxy :: SProxy noR)(_i <<< _ProductSecond)

-- Ctor > Arg > Product > Get
class GenericCtorArgProductGet (lenses :: # Type) s (a :: # Type) where
  _GenericCtorArgProductGet :: Record lenses -> s -> Maybe (Record a)

instance _GenericCtorArgProductGetImpl
  :: ( RowToList a aRl
    , GenericCtorArgProductGetRL aRl lenses s () a
    )
  => GenericCtorArgProductGet lenses s a
  where
    _GenericCtorArgProductGet lenses s
      = map (Builder.build <@> {})
      $ _GenericCtorArgProductGetRL (RLProxy :: RLProxy aRl) lenses s

class GenericCtorArgProductGetRL (aRl :: RowList) (lenses :: # Type) s (from :: # Type) (to :: # Type) | aRl lenses s from -> to
  where
    _GenericCtorArgProductGetRL :: RLProxy aRl -> Record lenses -> s -> Maybe (Builder (Record from) (Record to))

instance _GenericCtorArgProductGetRLNil
  :: GenericCtorArgProductGetRL RowList.Nil lenses s from from
  where
    _GenericCtorArgProductGetRL _ lenses s = Just identity

instance _GenericCtorArgProductGetRLCons
  :: ( GenericCtorArgProductGetRL restRl lenses s from to1
    , IsSymbol label
    , Row.Lacks label to1
    , Row.Cons label typ to1 to
    , Row.Cons label (p typ typ -> p s s) restLenses lenses -- NOTE
    )
  => GenericCtorArgProductGetRL (RowList.Cons label typ restRl) lenses s from to
  where
    _GenericCtorArgProductGetRL _ lenses s
      = lift2 (<<<)
          ( Builder.insert (SProxy :: SProxy label)
                    -- NOTE not sure how to recover proof (Function ~ p)
           <$> s ^? (unsafeCoerce (Record.get (SProxy :: SProxy label) lenses))
          )
          ( _GenericCtorArgProductGetRL (RLProxy :: RLProxy restRl)
              lenses s
          )

-- Ctor > Arg > Product > Set
class GenericCtorArgProductSet (lenses :: # Type) s (a :: # Type) where
  _GenericCtorArgProductSet :: Record lenses -> s -> Record a -> s

instance _GenericCtorArgProductSetImpl
  :: ( RowToList a aRl
    , GenericCtorArgProductSetRL aRl lenses s a
    )
  => GenericCtorArgProductSet lenses s a
  where
    _GenericCtorArgProductSet lenses s a
      = _GenericCtorArgProductSetRL (RLProxy :: RLProxy aRl)
          lenses s a

class GenericCtorArgProductSetRL (aRl :: RowList) (lenses :: # Type) s (a :: # Type)
  where
    _GenericCtorArgProductSetRL :: RLProxy aRl -> Record lenses -> s -> Record a -> s

instance _GenericCtorArgProductSetRLNil
  :: GenericCtorArgProductSetRL RowList.Nil lenses s a
  where
    _GenericCtorArgProductSetRL _ _ s _ = s

instance _GenericCtorArgProductSetRLCons
  :: ( GenericCtorArgProductSetRL restRl lenses s a
    , IsSymbol label
    , Row.Cons label (p typ typ -> p s s) restLenses lenses -- NOTE Function ~ p
    , Row.Cons label typ restA a
    )
  => GenericCtorArgProductSetRL (RowList.Cons label typ restRl) lenses s a
  where
    _GenericCtorArgProductSetRL _ lenses s a =
      _GenericCtorArgProductSetRL (RLProxy :: RLProxy restRl)
        lenses
              -- NOTE not sure how to recover proof (Forget (First typ) ~ p)
        (s # (unsafeCoerce (Record.get (SProxy :: SProxy label) lenses))
           .~ (Record.get (SProxy :: SProxy label) a)
        )
        a

-----------------
-- GenericNewtype

class GenericNewtype p s i o | p s i -> o where
  genericNewtype :: Optic' p s i -> o

instance genericNewtypeImpl
  :: ( Newtype i i'
    , Profunctor p
    , GenericTypeSort p s i' o
    )
  => GenericNewtype p s i o
  where
    genericNewtype _i =
      genericTypeSort (Proxy :: Proxy i')
        (_i <<< _Newtype)

---------------
-- GenericIndex

class GenericIndex p s i o | p s i -> o where
  genericIndex :: Optic' p s i -> o

instance genericIndexImpl
  :: ( Index i a b
    , Wander p
    , GenericTypeSort p s b o
    )
  => GenericIndex p s i (a -> { ix :: o, ix_ :: p b b -> p s s })
  where
    genericIndex _i a =
      { ix: genericTypeSort (Proxy :: Proxy b)
              (_i <<< ix a)
      , ix_: _i <<< ix a
      }

------------
-- GenericAt

class GenericAt p s i o | p s i -> o where
  genericAt :: Optic' p s i -> o

instance genericAtImpl
  :: ( At i a b
    , Index i a b
    , Wander p
    , GenericTypeSort p s (Maybe b) o1
    , GenericTypeSort p s b o2
    )
  => GenericAt p s i (a -> { at :: o1, ix :: o2, at_ :: p (Maybe b) (Maybe b) -> p s s, ix_ :: p b b -> p s s })
  where
    genericAt _i a =
      { at: genericTypeSort (Proxy :: Proxy (Maybe b)) (_i <<< at a)
      , at_: _i <<< at a
      , ix: genericTypeSort (Proxy :: Proxy b) (_i <<< ix a)
      , ix_: _i <<< ix a
      }

--------------------
-- GenericLensRecord

class GenericLensRecord p s (i :: # Type) (o :: # Type) | i -> o where
  genericLensRecord :: Optic' p s (Record i) -> Record o

instance genericLensRecordImpl
  :: ( RowToList i iRl
    , GenericLensRL p s i iRl o
    )
  => GenericLensRecord p s i o
  where
    genericLensRecord _i
      = Builder.build <@> {}
      $ genericLensRL (RLProxy :: RLProxy iRl) _i

class GenericLensRL p s i (iRl :: RowList) (to :: # Type) | p s i iRl -> to where
  genericLensRL :: RLProxy iRl -> Optic' p s (Record i) -> Builder {} (Record to)

instance genericLensRLNil :: GenericLensRL p s i (RowList.Nil) () where
  genericLensRL _ _ = identity

instance genericLensRLCons
  :: ( IsSymbol label
    , Row.Cons label typ r i
    , Strong p
    , GenericTypeSort p s typ o
    , Row.Lacks label restTo
    , Row.Cons label o restTo to1

    , Symbol.Append label "_" label_
    , IsSymbol label_
    , Row.Lacks label_ to1
    , Row.Cons label_ (p typ typ -> p s s) to1 to

    , GenericLensRL p s i restRl restTo
    )
  => GenericLensRL p s i (RowList.Cons label typ restRl) to
  where
    genericLensRL _ _i
      = Builder.insert (SProxy :: SProxy label_)
          (_i <<< prop (SProxy :: SProxy label))
      <<< Builder.insert (SProxy :: SProxy label)
          ( genericTypeSort (Proxy :: Proxy typ)
              (_i <<< prop (SProxy :: SProxy label))
          )
      <<< genericLensRL (RLProxy :: RLProxy restRl) _i

------------------
-- GenericPrismSum

class GenericPrismSum p s i (o :: # Type) | p s i -> o where
  genericPrismSum :: Optic' p s i -> Record o

instance genericPrismSumImpl
  :: ( GenericPrismSumCtor p s iRep () o
    , Generic i iRep
    , Profunctor p
    )
  => GenericPrismSum p s i o
  where
    genericPrismSum _i
      = Builder.build <@> {}
      $ genericPrismSumCtor (_i <<< _Generic')

-- Ctor
class GenericPrismSumCtor p s ctor (from :: # Type) (to :: # Type)
  | p s ctor from -> to
  where
    genericPrismSumCtor :: Optic' p s ctor -> Builder (Record from) (Record to)

instance genericPrismSumCtorConstructor
  :: ( Symbol.Append "_" label _label
    , GenericPrismSumArg p s arg _label from to
    , Profunctor p
    )
  => GenericPrismSumCtor p s (Constructor label arg) from to
  where
    genericPrismSumCtor _i
      = genericPrismSumArg (SProxy :: SProxy _label) (_i <<< _Constructor)

instance genericPrismSumCtorSum
  :: ( GenericPrismSumCtor p s ctorR from to1
    , GenericPrismSumCtor p s ctorL to1 to
    , Choice p
    )
  => GenericPrismSumCtor p s (Sum ctorL ctorR) from to where
    genericPrismSumCtor _i
      = genericPrismSumCtor (_i <<< _SumInl)
      <<< genericPrismSumCtor (_i <<< _SumInr)

-- Ctor > Arg
class GenericPrismSumArg p s arg (_label :: Symbol) (from :: # Type) (to :: # Type) | p s arg _label from -> to where
  genericPrismSumArg :: SProxy _label -> Optic' p s arg -> Builder (Record from) (Record to)

instance genericPrismSumArgNoArguments
  :: ( Symbol.Append _label "_" _label_
    , IsSymbol _label_
    , Row.Lacks _label_ from
    , Row.Cons _label_ (p Unit Unit -> p s s) from to
    , Profunctor p
    )
  => GenericPrismSumArg p s NoArguments _label from to
  where
    genericPrismSumArg _ _i =
      Builder.insert (SProxy :: SProxy _label_) (_i <<< _NoArguments)

instance genericPrismSumArgArgument
  :: ( Profunctor p
    , GenericTypeSort p s a o
    , IsSymbol _label
    , Row.Lacks _label from
    , Row.Cons _label o from to1

    , Symbol.Append _label "_" _label_
    , IsSymbol _label_
    , Row.Lacks _label_ to1
    , Row.Cons _label_ (p a a -> p s s) to1 to
    )
  => GenericPrismSumArg p s (Argument a) _label from to
  where
    genericPrismSumArg _label _i
      = Builder.insert (SProxy :: SProxy _label_)
          (_i <<< _Argument)
      <<< Builder.insert _label
          (genericTypeSort (Proxy :: Proxy a) (_i <<< _Argument))

instance genericPrismSumArgProductImpl
  :: ( GenericPrismSumArgProduct p s (Product l r) "1" () o
    , IsSymbol _label
    , Row.Lacks _label from
    , Row.Cons _label (Record o) from to1

    , Symbol.Append _label "_" _label_
    , GenericCtorArgProduct p s (Product l r) a
    , IsSymbol _label_
    , Row.Lacks _label_ to1
    , Row.Cons _label_ (p (Record a) (Record a) -> p s s) to1 to
    )
  => GenericPrismSumArg p s (Product l r) _label from to
  where
    genericPrismSumArg _label _i
      = Builder.insert (SProxy :: SProxy _label_)
          (_GenericCtorArgProduct _i)
      <<< Builder.insert _label
          (Builder.build <@> {}
            $ genericPrismSumArgProduct
                (Proxy :: Proxy (Product l r))
                (SProxy :: SProxy "1")
                _i)

-- Ctor > Arg > Product
class GenericPrismSumArgProduct p s arg (no :: Symbol) (from :: # Type) (to :: # Type)
  | p s arg no from -> to
  where
    genericPrismSumArgProduct
      :: Proxy arg
      -> SProxy no
      -> Optic' p s arg
      -> Builder (Record from) (Record to)

instance genericPrismSumArgProductArgument
  :: ( Symbol.Append "_" no _label
    , GenericTypeSort p s a o
    , IsSymbol _label
    , Row.Lacks _label from
    , Row.Cons _label o from to1

    , Symbol.Append _label "_" _label_
    , IsSymbol _label_
    , Row.Lacks _label_ to1
    , Row.Cons _label_ (p a a -> p s s) to1 to

    , Profunctor p
    )
  => GenericPrismSumArgProduct p s (Argument a) no from to
  where
    genericPrismSumArgProduct _ _ _i
      = Builder.insert (SProxy :: SProxy _label_)
          (_i <<< _Argument)
      <<< Builder.insert (SProxy :: SProxy _label)
          (genericTypeSort (Proxy :: Proxy a) (_i <<< _Argument))

instance genericPrismSumArgProductProduct
  :: ( GenericPrismSumArgProduct p s r noR from to1
    , GenericPrismSumArgProduct p s l noL to1 to
    , Succ noL noR
    , Strong p
    )
  => GenericPrismSumArgProduct p s (Product l r) noL from to
  where
    genericPrismSumArgProduct _ _ _i
      = genericPrismSumArgProduct
          (Proxy :: Proxy l)
          (SProxy :: SProxy noL)
          (_i <<< _ProductFirst)
      <<< genericPrismSumArgProduct
          (Proxy :: Proxy r)
          (SProxy :: SProxy noR)
          (_i <<< _ProductSecond)

------------------
-- GenericTypeSort

-- NOTE foreign import kind TType
data TScalar
data TRecord
data TIndex
data TAt
data TSum
data TNewtype

class TTypeFamily t tt | t -> tt
instance ttypeFamilyInt :: TTypeFamily Int TScalar
instance ttypeFamilyNumber :: TTypeFamily Number TScalar
instance ttypeFamilyString :: TTypeFamily String TScalar
instance ttypeFamilyChar :: TTypeFamily Char TScalar
instance ttypeFamilyBoolean :: TTypeFamily Boolean TScalar
instance ttypeFamilyRecord :: TTypeFamily (Record a) TRecord
instance ttypeFamilyArray :: TTypeFamily (Array a) TIndex
instance ttypeFamilySet :: TTypeFamily (Set v) TAt
instance ttypeFamilyMap :: TTypeFamily (Map k v) TAt
instance ttypeFamilyMaybe :: TTypeFamily (Maybe a) TSum
instance ttypeFamilyNewtype :: TTypeFamily (Identity a) TNewtype

class GenericTypeSort p s i o | p s i -> o where
  genericTypeSort :: Proxy i -> Optic' p s i -> o

instance genericTypeSortImpl
  :: ( TTypeFamily i tt
    , GenericTypeDispatch p s i tt o
    )
  => GenericTypeSort p s i o
  where
    genericTypeSort _ _i
      = genericTypeDispatch
          (Proxy :: Proxy i)
          (Proxy :: Proxy tt)
          _i

class GenericTypeDispatch p s i tt o | p s i -> o where
  genericTypeDispatch :: Proxy i -> Proxy tt -> Optic' p s i -> o

instance genericTypeDispatchScalar
  :: GenericTypeDispatch p s i TScalar (p i i -> p s s)
  where
    genericTypeDispatch _ _ _i = _i

instance genericTypeDispatchRecord
  :: GenericLensRecord p s i o
  => GenericTypeDispatch p s (Record i) TRecord (Record o)
  where
    genericTypeDispatch _ _ _i = genericLensRecord _i

instance genericTypeDispatchIndex
  :: GenericIndex p s i o
  => GenericTypeDispatch p s i TIndex o
  where
    genericTypeDispatch _ _ _i = genericIndex _i

instance genericTypeDispatchAt
  :: GenericAt p s i o
  => GenericTypeDispatch p s i TAt o
  where
    genericTypeDispatch _ _ _i = genericAt _i

instance genericTypeDispatchSum
  :: GenericPrismSum p s i o
  => GenericTypeDispatch p s i TSum (Record o)
  where
    genericTypeDispatch _ _ _i = genericPrismSum _i

instance genericTypeDispatchNewtype
  :: GenericNewtype p s i o
  => GenericTypeDispatch p s i TNewtype o
  where
    genericTypeDispatch _ _ _i = genericNewtype _i
