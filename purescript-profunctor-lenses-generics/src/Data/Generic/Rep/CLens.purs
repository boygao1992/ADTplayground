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
import Data.Identity (Identity)
import Data.Lens
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Tuple
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Unsafe.Coerce

infixr 2 type Beside as <>

genericLens :: forall s o. GenericTypeSort COptic s s o => Proxy s -> o
genericLens _ = genericTypeSort (COptic identity :: COptic s s s s)

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

class (CLenses li, CLenses lo) <=
  GenericCtor ctor li s i lo a | ctor li s i -> lo a where
  _GenericCtor :: SProxy ctor -> li s s i i -> lo s s a a

instance genericCtorSumFound ::
  ( CCompose li CPrism li1
  , GenericCtorArg li1 s arg lo a
  )
  => GenericCtor ctor li s (Sum (Constructor ctor arg) r) lo a where
  _GenericCtor ctor _i = _GenericCtorArg (_i .<<< _SumInl .<<< _Constructor)
else
instance genericCtorSumNext ::
  ( CCompose li CPrism li1
  , GenericCtor ctor li1 s r lo a
  )
  => GenericCtor ctor li s (Sum l r) lo a where
  _GenericCtor ctor _i = _GenericCtor ctor (_i .<<< _SumInr)
else
instance genericCtorSumLast ::
  ( CCompose li CIso li1
  , GenericCtorArg li1 s arg lo a
  )
  => GenericCtor ctor li s (Constructor ctor arg) lo a where
  _GenericCtor ctor _i = _GenericCtorArg (_i .<<< _Constructor)
else
instance genericCtorSumFail ::
  ( Fail
    ( Text "No constructors found called `" <>
      Text ctor <>
      Text "`"
    )
  , CLenses li
  , CLenses lo
  )
  => GenericCtor ctor li s (Constructor other b) lo a where
  _GenericCtor _ = unsafeCoerce

-- Ctor > Arg
class (CLenses li, CLenses lo) <=
  GenericCtorArg li s arg lo a | li s arg -> lo a where
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
  => GenericCtorArg li s (Product l r) CTraversal (Record a) where
  _GenericCtorArg = _GenericCtorArgProduct

-- Ctor > Arg > Product
class CLenses li <=
  GenericCtorArgProduct li s arg (a :: # Type) | li s arg -> a where
  _GenericCtorArgProduct :: li s s arg arg -> CTraversal s s (Record a) (Record a)

instance genericCtorArgProductImpl ::
  ( CLenses li
  , GenericCtorArgProductType "1" (Product l r) () a
  , GenericCtorArgProductLenses "1" li s (Product l r) () lenses
  , GenericCtorArgProductGet lenses s a
  , GenericCtorArgProductSet lenses s a
  )
  => GenericCtorArgProduct li s (Product l r) a where
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
class CLenses li <=
  GenericCtorArgProductLenses (no :: Symbol) li s i (from :: # Type) (to :: # Type)
  | no li s i from -> to where
  _GenericCtorArgProductLenses
    :: SProxy no -> li s s i i -> Builder (Record from) (Record to)

instance _GenericCtorArgProductArgument ::
  ( Symbol.Append "_" no _label
  , CCompose li CIso lo
  , IsSymbol _label
  , Row.Lacks _label from
  , Row.Cons _label (lo s s a a) from to
  )
  => GenericCtorArgProductLenses no li s (Argument a) from to where
  _GenericCtorArgProductLenses _ _i
    = Builder.insert (SProxy :: SProxy _label)
      (_i .<<< _Argument)

instance _GenericCtorArgProductProduct ::
  ( Succ noL noR
  , CCompose li CLens lo
  , GenericCtorArgProductLenses noR lo s r from to1
  , GenericCtorArgProductLenses noL lo s l to1 to
  )
  => GenericCtorArgProductLenses noL li s (Product l r) from to where
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
  => GenericCtorArgProductGet lenses s a where
  _GenericCtorArgProductGet lenses s
    = map (Builder.build <@> {})
      $ _GenericCtorArgProductGetRL (RLProxy :: RLProxy aRl) lenses s

class GenericCtorArgProductGetRL
  (aRl :: RowList) (lenses :: # Type) s (from :: # Type) (to :: # Type)
  | aRl lenses s from -> to where
  _GenericCtorArgProductGetRL
    :: RLProxy aRl -> Record lenses -> s -> Maybe (Builder (Record from) (Record to))

instance _GenericCtorArgProductGetRLNil ::
  GenericCtorArgProductGetRL RowList.Nil lenses s from from where
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
  => GenericCtorArgProductSet lenses s a where
  _GenericCtorArgProductSet lenses s a =
    _GenericCtorArgProductSetRL (RLProxy :: RLProxy aRl) lenses s a

class GenericCtorArgProductSetRL (aRl :: RowList) (lenses :: # Type) s (a :: # Type)
  where
    _GenericCtorArgProductSetRL :: RLProxy aRl -> Record lenses -> s -> Record a -> s

instance _GenericCtorArgProductSetRLNil ::
  GenericCtorArgProductSetRL RowList.Nil lenses s a where
  _GenericCtorArgProductSetRL _ _ s _ = s

instance _GenericCtorArgProductSetRLCons ::
  ( GenericCtorArgProductSetRL restRl lenses s a
  , IsSymbol label
  , Row.Cons label (l s s typ typ) restLenses lenses -- DONE
  , Row.Cons label typ restA a
  , CSet l
  )
  => GenericCtorArgProductSetRL (RowList.Cons label typ restRl) lenses s a where
  _GenericCtorArgProductSetRL _ lenses s a =
    _GenericCtorArgProductSetRL (RLProxy :: RLProxy restRl)
      lenses
      (s # (Record.get (SProxy :: SProxy label) lenses)
      ..~ (Record.get (SProxy :: SProxy label) a)
      )
      a

-----------------
-- GenericNewtype

class CLenses li <=
  GenericNewtype li s i o | li s i -> o where
  genericNewtype :: li s s i i -> o

instance genericNewtypeImpl ::
  ( Newtype i i'
  , CCompose li CIso li1
  , GenericTypeSort li1 s i' o
  )
  => GenericNewtype li s i o where
  genericNewtype _i =
    genericTypeSort (_i .<<< CIso _Newtype :: li1 s s i' i')

---------------
-- GenericIndex

class CLenses li <=
  GenericIndex li s i o | li s i -> o where
  genericIndex :: li s s i i -> o

instance genericIndexImpl ::
  ( Index i a b
  , Wander p
  , GenericTypeSort li1 s b o
  , CCompose li CTraversal li1
  )
  => GenericIndex li s i (a -> { ix :: o, ix_ :: li1 s s b b }) where
  genericIndex _i a =
    let
      _i1 = _i .<<< CTraversal (ix a)
    in
      { ix: genericTypeSort _i1
      , ix_: _i1
      }

------------
-- GenericAt

class CLenses li <=
  GenericAt li s i o | li s i -> o where
  genericAt :: li s s i i -> o

instance genericAtImpl ::
  ( At i a b
  , CCompose li CLens lat
  , GenericTypeSort lat s (Maybe b) o1
  , CCompose li CTraversal lix
  , GenericTypeSort lix s b o2
  )
  => GenericAt li s i (a -> { at :: o1, ix :: o2, at_ :: lat s s (Maybe b) (Maybe b), ix_ :: lix s s b b})
  where
    genericAt _i a =
      let
        _at = _i .<<< CLens (at a)
        _ix = _i .<<< CTraversal (ix a)
      in
        { at: genericTypeSort _at
        , at_: _at
        , ix: genericTypeSort _ix
        , ix_: _ix
        }

--------------------
-- GenericLensRecord

class CLenses li <=
  GenericLensRecord li s (i :: # Type) (o :: # Type) | li -> o where
  genericLensRecord :: li s s (Record i) (Record i) -> Record o

instance genericLensRecordImpl ::
  ( RowToList i iRl
  , GenericLensRL p s i iRl o
  )
  => GenericLensRecord p s i o where
  genericLensRecord _i
    = Builder.build <@> {}
      $ genericLensRL (RLProxy :: RLProxy iRl) _i

class CLenses li <=
  GenericLensRL li s i (iRl :: RowList) (to :: # Type) | li s i iRl -> to where
  genericLensRL
    :: RLProxy iRl -> li s s (Record i) (Record i) -> Builder {} (Record to)

instance genericLensRLNil ::
  CLenses li => GenericLensRL li s i (RowList.Nil) () where
  genericLensRL _ _ = identity

instance genericLensRLCons ::
  ( CCompose li CLens li1

  , Symbol.Append label "_" label_
  , IsSymbol label_
  , Row.Lacks label_ to1
  , Row.Cons label_ (li1 s s typ typ) to1 to

  , IsSymbol label
  , Row.Cons label typ r i
  , Row.Lacks label restTo
  , GenericTypeSort li1 s typ o
  , Row.Cons label o restTo to1

  , GenericLensRL li s i restRl restTo
  )
  => GenericLensRL li s i (RowList.Cons label typ restRl) to where
  genericLensRL _ _i =
    let
      _i' = _i .<<< CLens (prop (SProxy :: SProxy label))
    in
      Builder.insert (SProxy :: SProxy label_) _i'
      <<< Builder.insert (SProxy :: SProxy label)
          ( genericTypeSort _i' )
      <<< genericLensRL (RLProxy :: RLProxy restRl) _i

------------------
-- GenericPrismSum

class CLenses li <=
  GenericPrismSum li s i (o :: # Type) | li s i -> o where
  genericPrismSum :: li s s i i -> Record o

instance genericPrismSumImpl ::
  ( CCompose li CIso li1
  , GenericPrismSumCtor li1 s iRep () o
  , Generic i iRep
  )
  => GenericPrismSum li s i o where
  genericPrismSum _i
    = Builder.build <@> {}
      $ genericPrismSumCtor (_i .<<< _Generic')


-- Ctor
class CLenses li <=
  GenericPrismSumCtor li s ctor (from :: # Type) (to :: # Type)
  | li s ctor from -> to where
  genericPrismSumCtor :: li s s ctor ctor -> Builder (Record from) (Record to)

instance genericPrismSumCtorConstructor ::
  ( CCompose li CIso li1
  , Symbol.Append "_" label _label
  , GenericPrismSumArg li1 s arg _label from to
  )
  => GenericPrismSumCtor li s (Constructor label arg) from to where
  genericPrismSumCtor _i
    = genericPrismSumArg (SProxy :: SProxy _label) (_i .<<< _Constructor)

instance genericPrismSumCtorSum ::
  ( CCompose li CPrism li1
  , GenericPrismSumCtor li1 s ctorR from to1
  , GenericPrismSumCtor li1 s ctorL to1 to
  )
  => GenericPrismSumCtor li s (Sum ctorL ctorR) from to where
  genericPrismSumCtor _i
    = genericPrismSumCtor (_i .<<< _SumInl)
    <<< genericPrismSumCtor (_i .<<< _SumInr)


-- Ctor > Arg
class CLenses li <=
  GenericPrismSumArg li s arg (_label :: Symbol) (from :: # Type) (to :: # Type)
  | li s arg _label from -> to where
  genericPrismSumArg
    :: SProxy _label -> li s s arg arg -> Builder (Record from) (Record to)

instance genericPrismSumArgNoArguments ::
  ( CCompose li CIso li1
  , Symbol.Append _label "_" _label_
  , IsSymbol _label_
  , Row.Lacks _label_ from
  , Row.Cons _label_ (li1 s s Unit Unit) from to
  )
  => GenericPrismSumArg li s NoArguments _label from to where
  genericPrismSumArg _ _i =
    Builder.insert (SProxy :: SProxy _label_) (_i .<<< _NoArguments)

instance genericPrismSumArgArgument ::
  ( CCompose li CIso li1

  , Symbol.Append _label "_" _label_
  , IsSymbol _label_
  , Row.Lacks _label_ to1
  , Row.Cons _label_ (li1 s s a a) to1 to

  , IsSymbol _label
  , Row.Lacks _label from
  , GenericTypeSort li1 s a o
  , Row.Cons _label o from to1
  )
  => GenericPrismSumArg li s (Argument a) _label from to where
    genericPrismSumArg _label _i =
      let
        _i' = _i .<<< _Argument
      in
        Builder.insert (SProxy :: SProxy _label_) _i'
        <<< Builder.insert _label (genericTypeSort _i')

instance genericPrismSumArgProductImpl ::
  ( GenericPrismSumArgProduct li s (Product l r) "1" () o
  , IsSymbol _label
  , Row.Lacks _label from
  , Row.Cons _label (Record o) from to1

  , Symbol.Append _label "_" _label_
  , GenericCtorArgProduct li s (Product l r) a
  , IsSymbol _label_
  , Row.Lacks _label_ to1
  , Row.Cons _label_ (CTraversal s s (Record a) (Record a)) to1 to
  )
  => GenericPrismSumArg li s (Product l r) _label from to where
    genericPrismSumArg _label _i
      = Builder.insert (SProxy :: SProxy _label_)
          (_GenericCtorArgProduct _i)
      <<< Builder.insert _label
          ( Builder.build <@> {}
            $ genericPrismSumArgProduct
                (Proxy :: Proxy (Product l r))
                (SProxy :: SProxy "1")
                _i)

-- Ctor > Arg > Product
class CLenses li <=
  GenericPrismSumArgProduct li s arg (no :: Symbol) (from :: # Type) (to :: # Type)
  | li s arg no from -> to
  where
    genericPrismSumArgProduct
      :: Proxy arg -> SProxy no
      -> li s s arg arg -> Builder (Record from) (Record to)

instance genericPrismSumArgProductArgument ::
  ( CCompose li CIso li1

  , Symbol.Append _label "_" _label_
  , IsSymbol _label_
  , Row.Lacks _label_ to1
  , Row.Cons _label_ (li1 s s a a) to1 to -- TODO

  , Symbol.Append "_" no _label
  , GenericTypeSort li1 s a o
  , IsSymbol _label
  , Row.Lacks _label from
  , Row.Cons _label o from to1
  )
  => GenericPrismSumArgProduct li s (Argument a) no from to where
  genericPrismSumArgProduct _ _ _i =
    let
      _i' = _i .<<< _Argument
    in
      Builder.insert (SProxy :: SProxy _label_) _i'
      <<< Builder.insert (SProxy :: SProxy _label) (genericTypeSort _i')

instance genericPrismSumArgProductProduct ::
  ( GenericPrismSumArgProduct li1 s r noR from to1
  , GenericPrismSumArgProduct li1 s l noL to1 to
  , Succ noL noR
  , CCompose li CLens li1
  )
  => GenericPrismSumArgProduct li s (Product l r) noL from to where
  genericPrismSumArgProduct _ _ _i
    = genericPrismSumArgProduct (Proxy :: Proxy l) (SProxy :: SProxy noL)
        (_i .<<< _ProductFirst)
    <<< genericPrismSumArgProduct (Proxy :: Proxy r) (SProxy :: SProxy noR)
        (_i .<<< _ProductSecond)


------------------
-- GenericTypeSort

foreign import kind TType
foreign import data TScalar  :: TType
foreign import data TRecord  :: TType
foreign import data TIndex   :: TType
foreign import data TAt      :: TType
foreign import data TSum     :: TType
foreign import data TNewtype :: TType

data TTProxy (tt :: TType) = TTProxy

class TTypeFamily t (tt :: TType) | t -> tt
instance ttypeFamilyInt     :: TTypeFamily Int          TScalar
instance ttypeFamilyNumber  :: TTypeFamily Number       TScalar
instance ttypeFamilyString  :: TTypeFamily String       TScalar
instance ttypeFamilyChar    :: TTypeFamily Char         TScalar
instance ttypeFamilyBoolean :: TTypeFamily Boolean      TScalar
instance ttypeFamilyRecord  :: TTypeFamily (Record a)   TRecord
instance ttypeFamilyArray   :: TTypeFamily (Array a)    TIndex
instance ttypeFamilySet     :: TTypeFamily (Set v)      TAt
instance ttypeFamilyMap     :: TTypeFamily (Map k v)    TAt
instance ttypeFamilyMaybe   :: TTypeFamily (Maybe a)    TSum
instance ttypeFamilyNewtype :: TTypeFamily (Identity a) TNewtype

class CLenses li <=
  GenericTypeSort li s i o | li s i -> o where
  genericTypeSort :: li s s i i -> o

instance genericTypeSortImpl ::
  ( TTypeFamily i tt
  , GenericTypeDispatch tt li s i o
  )
  => GenericTypeSort li s i o where
  genericTypeSort _i
    = genericTypeDispatch (TTProxy :: TTProxy tt) _i

class CLenses li <=
  GenericTypeDispatch (tt :: TType) li s i o | tt li s i -> o where
  genericTypeDispatch :: TTProxy tt -> li s s i i -> o

instance genericTypeDispatchScalar ::
  CLenses li
  => GenericTypeDispatch TScalar li s i (li s s i i) where
  genericTypeDispatch _ _i = _i

instance genericTypeDispatchRecord ::
  GenericLensRecord li s i o
  => GenericTypeDispatch TRecord li s (Record i) (Record o) where
    genericTypeDispatch _ _i = genericLensRecord _i

instance genericTypeDispatchIndex ::
  GenericIndex li s i o
  => GenericTypeDispatch TIndex li s i o where
    genericTypeDispatch _ _i = genericIndex _i

instance genericTypeDispatchAt ::
  GenericAt li s i o
  => GenericTypeDispatch TAt li s i o where
  genericTypeDispatch _ _i = genericAt _i

instance genericTypeDispatchSum ::
  GenericPrismSum li s i o
  => GenericTypeDispatch TSum li s i (Record o) where
  genericTypeDispatch _ _i = genericPrismSum _i

instance genericTypeDispatchNewtype ::
  GenericNewtype li s i o
  => GenericTypeDispatch TNewtype li s i o where
    genericTypeDispatch _ _i = genericNewtype _i
