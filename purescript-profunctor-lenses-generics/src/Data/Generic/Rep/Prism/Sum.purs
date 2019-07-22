module Data.Generics.Rep.Prism.Sum where

import Prelude
import Type.Prelude
import Type.Proxy
import Num.Nat
import Prim.Symbol as Symbol
import Prim.RowList (kind RowList)
import Prim.RowList as RowList

import Data.Maybe
import Data.Set
import Data.Map
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, to)
import Data.Lens
import Data.Lens.Index (class Index, ix)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.TypeError (class Fail, Beside, Text)
import Record.Builder (Builder)
import Record.Builder as Builder
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
  | Three A B C
  | TestRecord { x :: { y :: { z :: Maybe A }} }
  | TestMap (Map String { a :: A })
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
    <\/> Constructor "TestRecord" (Argument { x :: { y :: { z :: Maybe A }}})
    <\/> Constructor "TestMap" (Argument (Map String { a :: A}))
    )
testsum = showGenericRep (Proxy :: Proxy TestSum)

-- NOTE
-- testsumPrism
--   :: forall p
--   . Profunctor p
--   => Wander p
--   => { _Empty :: Optic' p TestSum Unit
--     , _One :: { _A :: Optic' p TestSum Unit }
--     , _TestMap
--         :: String
--         -> { a :: { _A :: Optic' p TestSum Unit } }
--     , _TestRecord ::
--         { x :: { y :: { z :: { _Just :: { _A :: Optic' p TestSum Unit }
--                           , _Nothing :: Optic' p TestSum Unit
--                           }
--                     }
--               }
--         }
--     , _Three ::
--         { _1 :: { _A :: Optic' p TestSum Unit }
--         , _2 :: { _B :: Optic' p TestSum Unit }
--         , _3 :: { _C :: Optic' p TestSum Unit }
--         }
--     , _Two ::
--         { _1 :: { _A :: Optic' p TestSum Unit }
--         , _2 :: { _B :: Optic' p TestSum Unit }
--         }
--     }
-- testsumPrism = genericTypeSort (Proxy :: Proxy TestSum) identity

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

---------------
-- GenericIndex

class GenericIndex p s i o | p s i -> o where
  genericIndex :: Proxy i -> Optic' p s i -> o

instance genericIndexImpl
  :: ( Index i a b
    , Wander p
    , GenericTypeSort p s b o
    )
  => GenericIndex p s i (a -> o)
  where
    genericIndex _ _i a
      = genericTypeSort (Proxy :: Proxy b)
          (_i <<< ix a)

--------------------
-- GenericLensRecord

class GenericLensRecord p s (i :: # Type) (o :: # Type) | i -> o where
  genericLensRecord :: RProxy i -> Optic' p s (Record i) -> Record o

instance genericLensRecordImpl
  :: ( RowToList i iRl
    , GenericLensRL p s i iRl o
    )
  => GenericLensRecord p s i o
  where
    genericLensRecord _ _i
      = Builder.build <@> {}
      $ genericLensRL (RProxy :: RProxy i) (RLProxy :: RLProxy iRl) _i

class GenericLensRL p s i (iRl :: RowList) (to :: # Type) | p s i iRl -> to where
  genericLensRL :: RProxy i -> RLProxy iRl -> Optic' p s (Record i) -> Builder {} (Record to)

instance genericLensRLNil :: GenericLensRL p s i (RowList.Nil) () where
  genericLensRL _ _ _ = identity

instance genericLensRLCons
  :: ( IsSymbol label
    , Row.Cons label typ r i
    , Strong p
    , GenericTypeSort p s typ o
    , Row.Cons label o restTo to
    , Row.Lacks label restTo
    , GenericLensRL p s i restRl restTo
    )
  => GenericLensRL p s i (RowList.Cons label typ restRl) to
  where
    genericLensRL _ _ _i
      = Builder.insert (SProxy :: SProxy label)
          ( genericTypeSort (Proxy :: Proxy typ)
              (_i <<< prop (SProxy :: SProxy label))
          )
      <<< genericLensRL (RProxy :: RProxy i) (RLProxy :: RLProxy restRl) _i

------------------
-- GenericPrismSum

class GenericPrismSum p s i (o :: # Type) | p s i -> o where
  genericPrismSum :: Proxy i -> Optic' p s i -> Record o

instance genericPrismSumImpl
  :: ( GenericPrismSumCtor p s iRep () o
    , Generic i iRep
    , Profunctor p
    )
  => GenericPrismSum p s i o
  where
    genericPrismSum _ _i
      = Builder.build <@> {}
      $ genericPrismSumCtor (Proxy :: Proxy s) (Proxy :: Proxy iRep)
          (_i <<< _Generic')

class GenericPrismSumCtor p s ctor (from :: # Type) (to :: # Type)
  | p s ctor from -> to where
  genericPrismSumCtor
    :: Proxy s -> Proxy ctor
    -> Optic' p s ctor
    -> Builder (Record from) (Record to)

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
    genericPrismSumCtor _ _ _i
      = Builder.insert (SProxy :: SProxy _label)
        $ genericPrismSumArg
            (Proxy :: Proxy arg)
            (_i <<< _Constructor)

instance genericPrismSumCtorSum
  :: ( GenericPrismSumCtor p s ctorR from to1
    , GenericPrismSumCtor p s ctorL to1 to
    , Choice p
    )
  => GenericPrismSumCtor p s (Sum ctorL ctorR) from to where
    genericPrismSumCtor _ _ _i
      = genericPrismSumCtor (Proxy :: Proxy s) (Proxy :: Proxy ctorL)
          (_i <<< _SumInl)
      <<< genericPrismSumCtor (Proxy :: Proxy s) (Proxy :: Proxy ctorR)
          (_i <<< _SumInr)

class GenericPrismSumArg p s arg o | p s arg -> o where
  genericPrismSumArg :: Proxy arg -> Optic' p s arg -> o

instance genericPrismSumArgNoArguments
  :: Profunctor p
  => GenericPrismSumArg p s NoArguments (p Unit Unit -> p s s) where
    genericPrismSumArg _ _i = _i <<< _NoArguments

instance genericPrismSumArgArgument
  :: ( Profunctor p
    , GenericTypeSort p s a o
    )
  => GenericPrismSumArg p s (Argument a) o
  where
    genericPrismSumArg _ _i
      = genericTypeSort (Proxy :: Proxy a)
          (_i <<< _Argument)

instance genericPrismSumArgProductImpl
  :: GenericPrismSumArgProduct p s (Product l r) "1" () o
  => GenericPrismSumArg p s (Product l r) (Record o)
  where
    genericPrismSumArg _ _i =
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
      genericLensRecord (RProxy :: RProxy i) _i
else
instance genericTypeDispatchIndex
  :: GenericIndex p s i o
  => GenericTypeDispatch p s i TIndex o
  where
    genericTypeDispatch _ _ _i =
      genericIndex (Proxy :: Proxy i) _i
else
instance genericTypeDispatchSum
  :: GenericPrismSum p s i o
  => GenericTypeDispatch p s i TSum (Record o)
  where
    genericTypeDispatch _ _ _i =
      genericPrismSum (Proxy :: Proxy i) _i
