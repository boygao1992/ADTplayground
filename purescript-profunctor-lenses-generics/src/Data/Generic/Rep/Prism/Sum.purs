module Data.Generics.Rep.Prism.Sum where

import Prelude
import Prim.Row as Row
import Prim.TypeError (class Fail, Beside, Text)
import Type.Prelude
import Type.Proxy
import Unsafe.Coerce (unsafeCoerce)

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, to)
import Data.Lens
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Tuple (Tuple(..))
import Record.Builder (Builder)
import Record.Builder as Builder

infixr 2 type Beside as <>
infixr 2 type Sum as <\/>
infixr 2 type Product as </\>

newtype TestNewtype = TestNewtype { name :: String }
derive instance genericTestNewtype :: Generic TestNewtype _

testNewtype :: Proxy
  (Constructor "TestNewtype"
     (Argument
        { name :: String
        }
     )
  )
testNewtype = showGenericRep (Proxy :: Proxy TestNewtype)

data A
data B
data C

data TestSum
  = Empty
  | One A
  | Two A B
  | Three A B C
  | TestRecord { name :: String }
derive instance genericTestSum :: Generic TestSum _

class Generic a rep <= ShowGeneric a rep | a -> rep where
  showGenericRep :: Proxy a -> Proxy rep
instance showGeneircImpl :: Generic a rep => ShowGeneric a rep where
  showGenericRep _ = Proxy :: Proxy rep

testsum
  :: Proxy
    ( Constructor "Empty" NoArguments
    <\/> Constructor "One" (Argument A)
    <\/> Constructor "Two" (Argument A </\> Argument B)
    <\/> Constructor "Three" (Argument A </\> Argument B </\> Argument C)
    <\/> Constructor "TestRecord" (Argument { name :: String } )
    )
testsum = showGenericRep (Proxy :: Proxy TestSum)

type State rootTyp typ (r :: # Type) =
  { path :: Lens' rootTyp typ
  , result :: Record r
  }

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
-- _SumInl = prism Inl $ case _ of
--   Inl l -> Right l
--   Inr r -> Left (Inr r)

-- | Prism into the Inr of Sum
_SumInr :: forall l a b. Prism (Sum l a) (Sum l b) a b
_SumInr = _Sum <<< _Right
-- _SumInr = prism Inr $ case _ of
--   Inr r -> Right r
--   Inl l -> Left (Inl l)

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
-- _ProductFirst = lens (\(Product a _) -> a) (\(Product _ r) b -> Product b r)

-- | Lens into the second of a Product
_ProductSecond :: forall l a b. Lens (Product l a) (Product l b) a b
_ProductSecond = _Product <<< _2
-- _ProductSecond = lens (\(Product _ a) -> a) (\(Product l _) b -> Product l b)

-------
-- Ctor

_Ctor
  :: forall ctor s a p rep
  . Profunctor p
  => Generic s rep
  => GenericCtor p ctor rep a
  => SProxy ctor
  -> p a a -> p s s
_Ctor ctor = _Generic' <<< _GenericCtor ctor

class GenericCtor p ctor rep a | ctor rep -> a where
  _GenericCtor :: SProxy ctor -> p a a -> p rep rep

instance genericCtorSumFound
  :: ( Choice p
    , GenericCtorArg ctor p arg a
    )
  => GenericCtor p ctor (Sum (Constructor ctor arg) r) a
  where
    _GenericCtor ctor = _SumInl <<< _Constructor <<< _GenericCtorArg ctor
else
instance genericCtorSumNext
  :: ( Choice p
    , GenericCtor p ctor r a
    )
  => GenericCtor p ctor (Sum l r) a
  where
    _GenericCtor ctor = _SumInr <<< _GenericCtor ctor
else
instance genericCtorSumLast
  :: ( Profunctor p
    , GenericCtorArg ctor p arg a
    )
  => GenericCtor p ctor (Constructor ctor arg) a
  where
    _GenericCtor ctor = _Constructor <<< _GenericCtorArg ctor
else
instance genericCtorSumFail
  :: Fail
    ( Text "No constructors found called `"
    <> Text ctor
    <> Text "`"
    )
  => GenericCtor p ctor (Constructor other b) a
  where
    _GenericCtor _ = unsafeCoerce

class GenericCtorArg ctor p arg a | arg -> a where
  _GenericCtorArg :: SProxy ctor -> p a a -> p arg arg

instance genericCtorArgMatch
  :: Profunctor p
  => GenericCtorArg ctor p (Argument a) a
  where
    _GenericCtorArg _ = _Argument
else
instance genericCtorArgNone
  :: Profunctor p
  => GenericCtorArg ctor p NoArguments Unit
  where
    _GenericCtorArg _ = _NoArguments
else
instance genericCtorArgFail
  :: Fail
    ( Text "Multiple arguments found for constructor `"
    <> Text ctor
    <> Text "`"
    )
  => GenericCtorArg ctor p (Product l r) a
  where
    _GenericCtorArg x = unsafeCoerce

------------------
-- GenericPrismSum

class GenericPrismSum i (o :: # Type) | i -> o where
  genericPrismSum :: Proxy i -> Record o

instance genericPrismSumImpl
  :: ( GenericPrismSumCtor i p iRep () o
    , Generic i iRep
    , Profunctor p
    )
  => GenericPrismSum i o
  where
    genericPrismSum _ =
      (Builder.build <@> {})
      $ genericPrismSumCtor
          (Proxy :: Proxy i)
          (Proxy :: Proxy iRep)
          (_Generic' :: Optic' p i iRep)

class GenericPrismSumCtor i p ctor (from :: # Type) (to :: # Type)
  | i p ctor from -> to where
  genericPrismSumCtor
    :: Proxy i -> Proxy ctor
    -> Optic' p i ctor
    -> Builder (Record from) (Record to)

instance genericPrismSumCtorConstructor
  :: ( IsSymbol label
    , Row.Cons label (Optic' p i arg) from to
    , Row.Lacks label from
    , Profunctor p
    )
  => GenericPrismSumCtor i p (Constructor label arg) from to
  where
    genericPrismSumCtor _ _ _p =
      Builder.insert (SProxy :: SProxy label) (_p <<< _Constructor)

instance genericPrismSumCtorSum
  :: ( GenericPrismSumCtor i p ctorR from to1
    , GenericPrismSumCtor i p ctorL to1 to
    , Choice p
    )
  => GenericPrismSumCtor i p (Sum ctorL ctorR) from to where
    genericPrismSumCtor _ _ _p
      = genericPrismSumCtor (Proxy :: Proxy i) (Proxy :: Proxy ctorL)
          (_p <<< _SumInl)
      <<< genericPrismSumCtor (Proxy :: Proxy i) (Proxy :: Proxy ctorR)
          (_p <<< _SumInr)
