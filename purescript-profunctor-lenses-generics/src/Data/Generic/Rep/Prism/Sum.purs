module Data.Generics.Rep.Prism.Sum where

import Data.Lens
import Num.Nat
import Prelude
import Type.Prelude
import Type.Proxy

import Control.Apply (lift2)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, to)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Beside, Text)
import Record.Builder (Builder)
import Record.Builder as Builder
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

{-

- Lens, Strong =>
  - Record

- Indexed, Index =>
  - Array
  - List
  - Set
  - Map
  - Object

Generic =>
- Iso, Profunctor =>
  - Newtype
    - Identity
- Prism, Choice =>
  - Sum
    - Maybe

-}

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

data A = A
derive instance genericA :: Generic A _
data B = B
derive instance genericB :: Generic B _
data C = C
derive instance genericC :: Generic C _

data TestSum
  = Empty
  | One A
  | Two A B
  | Three A B C -- Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
  | TestRecord { x :: { y :: { z :: Maybe A } } }
  | TestMap (Map String { a :: A })
derive instance genericTestSum :: Generic TestSum _

_ThreeA :: Traversal' TestSum A
_ThreeA = _Generic' <<< _SumInr <<< _SumInr <<< _SumInr <<< _SumInl <<< _Constructor <<< _ProductFirst <<< _Argument

_ThreeB :: Traversal' TestSum B
_ThreeB = _Generic' <<< _SumInr <<< _SumInr <<< _SumInr <<< _SumInl <<< _Constructor <<< _ProductSecond <<< _ProductFirst <<< _Argument

_ThreeC :: Traversal' TestSum C
_ThreeC = _Generic' <<< _SumInr <<< _SumInr <<< _SumInr <<< _SumInl <<< _Constructor <<< _ProductSecond <<< _ProductSecond <<< _Argument

_ThreeLenses
  :: forall p
  . Wander p
  => { _1 :: Optic' p TestSum A
    , _2 :: Optic' p TestSum B
    , _3 :: Optic' p TestSum C
    }
_ThreeLenses =
  { _1: _ThreeA
  , _2: _ThreeB
  , _3: _ThreeC
  }

_Three :: forall p. Wander p => Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
_Three = wander _ThreeWander

_ThreeWander
  :: forall f
  . Applicative f
  => ({ _1 :: A, _2 :: B, _3 :: C } -> f { _1 :: A, _2 :: B, _3 :: C })
  -> TestSum
  -> f TestSum
_ThreeWander coalg s =
  fromMaybe (pure s)
  $ map (map $ _ThreeSet s)
  $ map coalg
  $ _ThreeGet s

_ThreeSet
  :: TestSum
  -> { _1 :: A
    , _2 :: B
    , _3 :: C
    }
  -> TestSum
_ThreeSet s a
  = s
    # _ThreeA .~ (Record.get (SProxy :: SProxy "_1") a)
    # _ThreeB .~ (Record.get (SProxy :: SProxy "_2") a)
    # _ThreeC .~ (Record.get (SProxy :: SProxy "_3") a)

_ThreeGet
  :: TestSum
  -> Maybe
     { _1 :: A
     , _2 :: B
     , _3 :: C
     }
_ThreeGet s
  = map (Builder.build <@> {})
  $ lift2 (<<<)
    ( Builder.insert (SProxy :: SProxy "_3")
      <$> s ^? (Record.get (SProxy :: SProxy "_3") _ThreeLenses)
    )
  $ lift2 (<<<)
    ( Builder.insert (SProxy :: SProxy "_2")
      <$> s ^? (Record.get (SProxy :: SProxy "_2") _ThreeLenses)
    )
  $ ( Builder.insert (SProxy :: SProxy "_1")
      <$> s ^? (Record.get (SProxy :: SProxy "_1") _ThreeLenses)
    )

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
    <\/> Constructor "TestRecord" (Argument { x :: { y :: { z :: Maybe A }}})
    <\/> Constructor "TestMap" (Argument (Map String { a :: A}))
    )
testsum = showGenericRep (Proxy :: Proxy TestSum)

-- NOTE
testsumPrism
  :: forall p
  . Profunctor p
  => Wander p
  => { _Empty :: Optic' p TestSum Unit
    , _One :: { _A :: Optic' p TestSum Unit }
    , _TestMap
        :: String
        -> { a :: { _A :: Optic' p TestSum Unit }
          , a_ :: Optic' p TestSum A
          }
    , _TestRecord ::
        { x :: { y :: { z :: { _Just :: { _A :: Optic' p TestSum Unit }
                          , _Nothing :: Optic' p TestSum Unit
                          }
                    , z_ :: Optic' p TestSum (Maybe A)
                    }
              , y_ :: Optic' p TestSum { z :: Maybe A }
              }
        , x_ :: Optic' p TestSum { y :: { z :: Maybe A } }
        }
    , _Three ::
        { _1 :: { _A :: Optic' p TestSum Unit }
        , _2 :: { _B :: Optic' p TestSum Unit }
        , _3 :: { _C :: Optic' p TestSum Unit }
        }
    , _Two ::
        { _1 :: { _A :: Optic' p TestSum Unit }
        , _2 :: { _B :: Optic' p TestSum Unit }
        }
    }
testsumPrism = genericTypeSort (Proxy :: Proxy TestSum) identity

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

class GenericCtorArg p s arg a | p s arg -> a where
  _GenericCtorArg :: Optic' p s arg -> Optic' p s a

instance genericCtorArgMatch ::
  Profunctor p =>
  GenericCtorArg p s (Argument a) a where
    _GenericCtorArg _i = _i <<< _Argument
else
instance genericCtorArgNone ::
  Profunctor p =>
  GenericCtorArg p s NoArguments Unit where
    _GenericCtorArg _i = _i <<< _NoArguments
else
instance genericCtorArgFail ::
  Fail (
    Text "Multiple arguments found for constructor `" <>
    Text "`" ) =>
  GenericCtorArg p s (Product l r) a where
    _GenericCtorArg _i = unsafeCoerce

-- TODO class GenericCtorArgProduct
-- class GenericCtorArgProduct 

---------------
-- GenericIndex

class GenericIndex p s i o | p s i -> o where
  genericIndex :: Optic' p s i -> o

instance genericIndexImpl
  :: ( Index i a b
    , Wander p
    , GenericTypeSort p s b o
    )
  => GenericIndex p s i (a -> o)
  where
    genericIndex _i a
      = genericTypeSort (Proxy :: Proxy b)
          (_i <<< ix a)

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

class GenericPrismSumCtor p s ctor (from :: # Type) (to :: # Type)
  | p s ctor from -> to where
  genericPrismSumCtor
    :: Optic' p s ctor -> Builder (Record from) (Record to)

instance genericPrismSumCtorConstructor
  :: ( Symbol.Append "_" label _label
    , IsSymbol _label
    , Row.Cons _label o from to
    , Row.Lacks _label from
    , Profunctor p
    , GenericPrismSumArg p s arg o
    )
  => GenericPrismSumCtor p s (Constructor label arg) from to
  where
    genericPrismSumCtor _i
      = Builder.insert (SProxy :: SProxy _label)
        $ genericPrismSumArg (_i <<< _Constructor)

instance genericPrismSumCtorSum
  :: ( GenericPrismSumCtor p s ctorR from to1
    , GenericPrismSumCtor p s ctorL to1 to
    , Choice p
    )
  => GenericPrismSumCtor p s (Sum ctorL ctorR) from to where
    genericPrismSumCtor _i
      = genericPrismSumCtor (_i <<< _SumInl)
      <<< genericPrismSumCtor (_i <<< _SumInr)

class GenericPrismSumArg p s arg o | p s arg -> o where
  genericPrismSumArg :: Optic' p s arg -> o

instance genericPrismSumArgNoArguments
  :: Profunctor p
  => GenericPrismSumArg p s NoArguments (p Unit Unit -> p s s) where
    genericPrismSumArg _i = _i <<< _NoArguments

instance genericPrismSumArgArgument
  :: ( Profunctor p
    , GenericTypeSort p s a o
    )
  => GenericPrismSumArg p s (Argument a) o
  where
    genericPrismSumArg _i
      = genericTypeSort (Proxy :: Proxy a)
          (_i <<< _Argument)

instance genericPrismSumArgProductImpl
  :: GenericPrismSumArgProduct p s (Product l r) "1" () o
  => GenericPrismSumArg p s (Product l r) (Record o)
  where
    genericPrismSumArg _i =
      Builder.build <@> {}
      $ genericPrismSumArgProduct
          (Proxy :: Proxy (Product l r))
          (SProxy :: SProxy "1")
          _i

class GenericPrismSumArgProduct p s arg (no :: Symbol) (from :: # Type) (to :: # Type)
  | p s arg no from -> to
  where
    genericPrismSumArgProduct
      :: Proxy arg
      -> SProxy no
      -> Optic' p s arg
      -> Builder (Record from) (Record to)

instance genericPrismSumArgProductArgument
  :: ( IsSymbol label
    , Profunctor p
    , Symbol.Append "_" no label
    , GenericTypeSort p s a o
    , Row.Cons label o from to
    , Row.Lacks label from
    )
  => GenericPrismSumArgProduct p s (Argument a) no from to
  where
    genericPrismSumArgProduct _ _ _i
      = Builder.insert (SProxy :: SProxy label)
        $ genericTypeSort (Proxy :: Proxy a)
            (_i <<< _Argument)

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
data TSum

class TTypeFamily t tt | t -> tt
instance ttypeFamilyInt :: TTypeFamily Int TScalar
else
instance ttypeFamilyNumber :: TTypeFamily Number TScalar
else
instance ttypeFamilyString :: TTypeFamily String TScalar
else
instance ttypeFamilyChar :: TTypeFamily Char TScalar
else
instance ttypeFamilyBoolean :: TTypeFamily Boolean TScalar
else
instance ttypeFamilyRecord :: TTypeFamily (Record a) TRecord
else
instance ttypeFamilyArray :: TTypeFamily (Array a) TIndex
else
instance ttypeFamilySet :: TTypeFamily (Set v) TIndex
else
instance ttypeFamilyMap :: TTypeFamily (Map k v) TIndex
else
instance ttypeFamilySum :: TTypeFamily a TSum

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
else
instance genericTypeDispatchRecord
  :: GenericLensRecord p s i o
  => GenericTypeDispatch p s (Record i) TRecord (Record o)
  where
    genericTypeDispatch _ _ _i =
      genericLensRecord _i
else
instance genericTypeDispatchIndex
  :: GenericIndex p s i o
  => GenericTypeDispatch p s i TIndex o
  where
    genericTypeDispatch _ _ _i =
      genericIndex _i
else
instance genericTypeDispatchSum
  :: GenericPrismSum p s i o
  => GenericTypeDispatch p s i TSum (Record o)
  where
    genericTypeDispatch _ _ _i =
      genericPrismSum _i
