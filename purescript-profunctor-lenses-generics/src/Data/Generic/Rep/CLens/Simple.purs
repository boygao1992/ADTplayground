module Data.Generic.Rep.CLens.Simple where

import Prelude
import Type.Prelude (class IsSymbol, class RowToList, Proxy(..), RLProxy(..), SProxy(..))

import Num.Nat (class Succ)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Beside, Text)

import Control.Apply (lift2)
import Data.Traversable (class Traversable)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, to)
import Data.Generic.Rep.Lens.Constraints.Simple (class CCompose', class CLenses', class CPreview', class CSet', CIso'(..), CLens'(..), COptic'(..), CPrism'(..), CTraversal'(..), (..~), (.<<<), (.^?))
import Data.Identity (Identity)
import Data.Lens (_1, _2, _Left, _Right, iso, traversed, wander)
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Unsafe.Coerce (unsafeCoerce)

infixr 2 type Beside as <>

genericLens :: forall s o. GenericTypeSort COptic' s s o => Proxy s -> o
genericLens _ = genericTypeSort (COptic' identity :: COptic' s s)

-----------------------------------
-- Primitive Lenses for Generic Rep

-- | Iso between a type and its rep.
_Generic' :: forall s sRep. Generic s sRep => CIso' s sRep
_Generic' = CIso' (iso from to)

-- | NoConstructors is equivalent to Void
_NoConstructors' :: CIso' NoConstructors Void
_NoConstructors' = CIso' (iso unsafeCoerce absurd)

-- | Iso between a Constructor and its argument rep
_Constructor'
  :: forall label1 arg1
  . CIso' (Constructor label1 arg1) arg1
_Constructor' = CIso' (iso (\(Constructor a) -> a) Constructor)

-- | Iso between Sum and Either
_Sum' :: forall a b . CIso' (Sum a b) (Either a b)
_Sum' = CIso'
  (iso
    (case _ of
      Inl a -> Left a
      Inr b -> Right b)
    (case _ of
      Left c -> Inl c
      Right d -> Inr d))

-- | Prism into the Inl of Sum
_Sum'Inl' :: forall a r. CPrism' (Sum a r) a
_Sum'Inl' = _Sum' .<<< CPrism' _Left

-- | Prism into the Inr of Sum
_Sum'Inr' :: forall l a. CPrism' (Sum l a) a
_Sum'Inr' = _Sum' .<<< CPrism' _Right

-- | Iso between NoArguments and Unit
_NoArguments' :: CIso' NoArguments Unit
_NoArguments' = CIso' (iso (const unit) (const NoArguments))

-- | Iso between Argument and its wrapped type
_Argument' :: forall a. CIso' (Argument a) a
_Argument' = CIso' (iso (\(Argument a) -> a) Argument)

-- | Iso between Product and Tuple
_Product' :: forall a b. CIso' (Product a b) (Tuple a b)
_Product' = CIso'
  (iso
    (\(Product a b) -> Tuple a b)
    (\(Tuple c d) -> Product c d))

-- | Lens into the first of a Product
_Product'First' :: forall a r. CLens' (Product a r) a
_Product'First' = _Product' .<<< CLens' _1

-- | Lens into the second of a Product
_Product'Second' :: forall l a. CLens' (Product l a) a
_Product'Second' = _Product' .<<< CLens' _2

-------
-- Ctor

_Ctor'
  :: forall ctor s a rep lo
  . Generic s rep
  => GenericCtor ctor CIso' s rep lo a
  => Proxy s
  -> SProxy ctor
  -> lo s a
_Ctor' _ ctor = _GenericCtor ctor _Generic'

class (CLenses' li, CLenses' lo) <=
  GenericCtor ctor li s i lo a | ctor li s i -> lo a where
  _GenericCtor :: SProxy ctor -> li s i -> lo s a

instance genericCtorSumFound ::
  ( CCompose' li CPrism' li1
  , GenericCtorArg li1 s arg lo a
  )
  => GenericCtor ctor li s (Sum (Constructor ctor arg) r) lo a where
  _GenericCtor ctor _i = _GenericCtorArg (_i .<<< _Sum'Inl' .<<< _Constructor')
else
instance genericCtorSumNext ::
  ( CCompose' li CPrism' li1
  , GenericCtor ctor li1 s r lo a
  )
  => GenericCtor ctor li s (Sum l r) lo a where
  _GenericCtor ctor _i = _GenericCtor ctor (_i .<<< _Sum'Inr')
else
instance genericCtorSumLast ::
  ( CCompose' li CIso' li1
  , GenericCtorArg li1 s arg lo a
  )
  => GenericCtor ctor li s (Constructor ctor arg) lo a where
  _GenericCtor ctor _i = _GenericCtorArg (_i .<<< _Constructor')
else
instance genericCtorSumFail ::
  ( Fail
    ( Text "No constructors found called `" <>
      Text ctor <>
      Text "`"
    )
  , CLenses' li
  , CLenses' lo
  )
  => GenericCtor ctor li s (Constructor other b) lo a where
  _GenericCtor _ = unsafeCoerce

-- Ctor > Arg
class (CLenses' li, CLenses' lo) <=
  GenericCtorArg li s arg lo a | li s arg -> lo a where
  _GenericCtorArg :: li s arg -> lo s a

instance genericCtorArgMatch ::
  CCompose' li CIso' lo
  => GenericCtorArg li s (Argument a) lo a where
  _GenericCtorArg _i = _i .<<< _Argument'

instance genericCtorArgNone ::
  CCompose' li CIso' lo
  => GenericCtorArg li s NoArguments lo Unit where
  _GenericCtorArg _i = _i .<<< _NoArguments'

instance genericCtorArgProduct ::
  GenericCtorArgProduct li s (Product l r) a
  => GenericCtorArg li s (Product l r) CTraversal' (Record a) where
  _GenericCtorArg = _GenericCtorArgProduct

-- Ctor > Arg > Product
class CLenses' li <=
  GenericCtorArgProduct li s arg (a :: # Type) | li s arg -> a where
  _GenericCtorArgProduct :: li s arg -> CTraversal' s (Record a)

instance genericCtorArgProductImpl ::
  ( CLenses' li
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
      CTraversal' (wander merge)

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
class CLenses' li <=
  GenericCtorArgProductLenses (no :: Symbol) li s i (from :: # Type) (to :: # Type)
  | no li s i from -> to where
  _GenericCtorArgProductLenses
    :: SProxy no -> li s i -> Builder (Record from) (Record to)

instance _GenericCtorArgProductArgument ::
  ( Symbol.Append "_" no _label
  , CCompose' li CIso' lo
  , IsSymbol _label
  , Row.Lacks _label from
  , Row.Cons _label (lo s a) from to
  )
  => GenericCtorArgProductLenses no li s (Argument a) from to where
  _GenericCtorArgProductLenses _ _i
    = Builder.insert (SProxy :: SProxy _label)
      (_i .<<< _Argument')

instance _GenericCtorArgProductProduct ::
  ( Succ noL noR
  , CCompose' li CLens' lo
  , GenericCtorArgProductLenses noR lo s r from to1
  , GenericCtorArgProductLenses noL lo s l to1 to
  )
  => GenericCtorArgProductLenses noL li s (Product l r) from to where
  _GenericCtorArgProductLenses _ _i
    = _GenericCtorArgProductLenses (SProxy :: SProxy noL) (_i .<<< _Product'First')
    <<< _GenericCtorArgProductLenses (SProxy :: SProxy noR)(_i .<<< _Product'Second')

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
  , Row.Cons label (l s typ) restLenses lenses -- DONE
  , CPreview' l
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
  , Row.Cons label (l s typ) restLenses lenses -- DONE
  , Row.Cons label typ restA a
  , CSet' l
  )
  => GenericCtorArgProductSetRL (RowList.Cons label typ restRl) lenses s a where
  _GenericCtorArgProductSetRL _ lenses s a =
    _GenericCtorArgProductSetRL (RLProxy :: RLProxy restRl)
      lenses
      (s # (Record.get (SProxy :: SProxy label) lenses)
      ..~ (Record.get (SProxy :: SProxy label) a)
      )
      a

-------------------
-- GenericTraversed

class (CLenses' li, Traversable f) <=
  GenericTraversed li s f i (from :: # Type) (to :: # Type) | li s f i from -> to
  where
    genericTraversed :: li s (f i) -> Builder (Record from) (Record to)

instance genericTraversedImpl ::
  ( Traversable f
  , CCompose' li CTraversal' li1
  , GenericTypeSort li1 s i o
  , Row.Lacks "traversed" from
  , Row.Lacks "traversed_" from
  )
  => GenericTraversed li s f i from ( traversed :: o, traversed_ :: li1 s i | from)
  where
  genericTraversed _i
    = Builder.insert (SProxy :: SProxy "traversed")
        (genericTypeSort (_i .<<< CTraversal' traversed :: li1 s i))
    <<< Builder.insert (SProxy :: SProxy "traversed_")
        (_i .<<< CTraversal' traversed :: li1 s i)

-----------------
-- GenericNewtype

class CLenses' li <=
  GenericNewtype li s i o | li s i -> o where
  genericNewtype :: li s i -> o

instance genericNewtypeImpl ::
  ( Newtype i i'
  , CCompose' li CIso' li1
  , GenericTypeSort li1 s i' o
  )
  => GenericNewtype li s i (li1 s i') where
  genericNewtype _i = _i .<<< CIso' _Newtype

---------------
-- GenericIndex

class CLenses' li <=
  GenericIndex li s i (from :: # Type) (to :: # Type) | li s i from -> to where
  genericIndex :: li s i -> Builder (Record from) (Record to)

instance genericIndexImpl ::
  ( Index i a b
  , GenericTypeSort li1 s b o
  , CCompose' li CTraversal' li1
  , Row.Lacks "ix" from
  , Row.Lacks "ix_" from
  )
  => GenericIndex li s i from (ix :: a -> o, ix_ :: a -> li1 s b | from) where
  genericIndex _i
    = Builder.insert (SProxy :: SProxy "ix")
        (\a -> genericTypeSort ( _i .<<< CTraversal' (ix a) :: li1 s b ))
    <<< Builder.insert (SProxy :: SProxy "ix_")
        (\a -> _i .<<< CTraversal' (ix a))

------------
-- GenericAt

class CLenses' li <=
  GenericAt li s i (from :: # Type) (to :: # Type) | li s i from -> to where
  genericAt :: li s i -> Builder (Record from) (Record to)

instance genericAtImpl ::
  ( At i a b
  , CCompose' li CLens' lat
  , GenericTypeSort lat s (Maybe b) o1
  , Row.Lacks "at" from
  , Row.Lacks "at_" from
  )
  => GenericAt li s i from (at :: a -> o1, at_ :: a -> lat s (Maybe b) | from) where
    genericAt _i
      = Builder.insert (SProxy :: SProxy "at")
          (\a -> genericTypeSort (_i .<<< CLens' (at a) :: lat s (Maybe b)))
      <<< Builder.insert (SProxy :: SProxy "at_")
          (\a -> (_i .<<< CLens' (at a) :: lat s (Maybe b)))

--------------------
-- GenericLensRecord

-- TODO Heterogeneous Mapping/Folding

class CLenses' li <=
  GenericLensRecord li s (i :: # Type) (from :: # Type) (to :: # Type)
  | li s i from -> to where
  genericLensRecord :: li s (Record i) -> Builder (Record from) (Record to)

instance genericLensRecordImpl ::
  ( RowToList i iRl
  , GenericLensRL p s i iRl from to
  )
  => GenericLensRecord p s i from to where
  genericLensRecord _i
    = genericLensRL (RLProxy :: RLProxy iRl) _i

class CLenses' li <=
  GenericLensRL li s i (iRl :: RowList) (from :: # Type) (to :: # Type)
  | li s i iRl from -> to where
  genericLensRL :: RLProxy iRl -> li s (Record i) -> Builder (Record from) (Record to)

instance genericLensRLNil ::
  CLenses' li => GenericLensRL li s i (RowList.Nil) from from where
  genericLensRL _ _ = identity

instance genericLensRLCons ::
  ( CCompose' li CLens' li1

  , Symbol.Append label "_" label_
  , IsSymbol label_
  , Row.Lacks label_ to1
  , Row.Cons label_ (li1 s typ) to1 to

  , IsSymbol label
  , Row.Cons label typ r i
  , Row.Lacks label restTo
  , GenericTypeSort li1 s typ o
  , Row.Cons label o restTo to1

  , GenericLensRL li s i restRl from restTo
  )
  => GenericLensRL li s i (RowList.Cons label typ restRl) from to where
  genericLensRL _ _i =
    let
      _i' = _i .<<< CLens' (prop (SProxy :: SProxy label))
    in
      Builder.insert (SProxy :: SProxy label_) _i'
      <<< Builder.insert (SProxy :: SProxy label)
          ( genericTypeSort _i' )
      <<< genericLensRL (RLProxy :: RLProxy restRl) _i

------------------
-- GenericPrismSum

class CLenses' li <=
  GenericPrismSum li s i (from :: # Type) (to :: # Type) | li s i from -> to where
  genericPrismSum :: li s i -> Builder (Record from) (Record to)

instance genericPrismSumImpl ::
  ( CCompose' li CIso' li1
  , GenericPrismSumCtor li1 s iRep from to
  , Generic i iRep
  )
  => GenericPrismSum li s i from to where
  genericPrismSum _i
    = genericPrismSumCtor (_i .<<< _Generic')

-- Ctor
class CLenses' li <=
  GenericPrismSumCtor li s ctor (from :: # Type) (to :: # Type)
  | li s ctor from -> to where
  genericPrismSumCtor :: li s ctor -> Builder (Record from) (Record to)

instance genericPrismSumCtorConstructor ::
  ( CCompose' li CIso' li1
  , Symbol.Append "_" label _label
  , GenericPrismSumArg li1 s arg _label from to
  )
  => GenericPrismSumCtor li s (Constructor label arg) from to where
  genericPrismSumCtor _i
    = genericPrismSumArg (SProxy :: SProxy _label) (_i .<<< _Constructor')

instance genericPrismSumCtorSum ::
  ( CCompose' li CPrism' li1
  , GenericPrismSumCtor li1 s ctorR from to1
  , GenericPrismSumCtor li1 s ctorL to1 to
  )
  => GenericPrismSumCtor li s (Sum ctorL ctorR) from to where
  genericPrismSumCtor _i
    = genericPrismSumCtor (_i .<<< _Sum'Inl')
    <<< genericPrismSumCtor (_i .<<< _Sum'Inr')


-- Ctor > Arg
class CLenses' li <=
  GenericPrismSumArg li s arg (_label :: Symbol) (from :: # Type) (to :: # Type)
  | li s arg _label from -> to where
  genericPrismSumArg
    :: SProxy _label -> li s arg -> Builder (Record from) (Record to)

instance genericPrismSumArgNoArguments ::
  ( CCompose' li CIso' li1
  , Symbol.Append _label "_" _label_
  , IsSymbol _label_
  , Row.Lacks _label_ from
  , Row.Cons _label_ (li1 s Unit) from to
  )
  => GenericPrismSumArg li s NoArguments _label from to where
  genericPrismSumArg _ _i =
    Builder.insert (SProxy :: SProxy _label_) (_i .<<< _NoArguments')

instance genericPrismSumArgArgument ::
  ( CCompose' li CIso' li1

  , Symbol.Append _label "_" _label_
  , IsSymbol _label_
  , Row.Lacks _label_ to1
  , Row.Cons _label_ (li1 s a) to1 to

  , IsSymbol _label
  , Row.Lacks _label from
  , GenericTypeSort li1 s a o
  , Row.Cons _label o from to1
  )
  => GenericPrismSumArg li s (Argument a) _label from to where
    genericPrismSumArg _label _i =
      let
        _i' = _i .<<< _Argument'
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
  , Row.Cons _label_ (CTraversal' s (Record a)) to1 to
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
class CLenses' li <=
  GenericPrismSumArgProduct li s arg (no :: Symbol) (from :: # Type) (to :: # Type)
  | li s arg no from -> to
  where
    genericPrismSumArgProduct
      :: Proxy arg -> SProxy no
      -> li s arg -> Builder (Record from) (Record to)

instance genericPrismSumArgProductArgument ::
  ( CCompose' li CIso' li1

  , Symbol.Append _label "_" _label_
  , IsSymbol _label_
  , Row.Lacks _label_ to1
  , Row.Cons _label_ (li1 s a) to1 to -- TODO

  , Symbol.Append "_" no _label
  , GenericTypeSort li1 s a o
  , IsSymbol _label
  , Row.Lacks _label from
  , Row.Cons _label o from to1
  )
  => GenericPrismSumArgProduct li s (Argument a) no from to where
  genericPrismSumArgProduct _ _ _i =
    let
      _i' = _i .<<< _Argument'
    in
      Builder.insert (SProxy :: SProxy _label_) _i'
      <<< Builder.insert (SProxy :: SProxy _label) (genericTypeSort _i')

instance genericPrismSumArgProductProduct ::
  ( GenericPrismSumArgProduct li1 s r noR from to1
  , GenericPrismSumArgProduct li1 s l noL to1 to
  , Succ noL noR
  , CCompose' li CLens' li1
  )
  => GenericPrismSumArgProduct li s (Product l r) noL from to where
  genericPrismSumArgProduct _ _ _i
    = genericPrismSumArgProduct (Proxy :: Proxy l) (SProxy :: SProxy noL)
        (_i .<<< _Product'First')
    <<< genericPrismSumArgProduct (Proxy :: Proxy r) (SProxy :: SProxy noR)
        (_i .<<< _Product'Second')


------------------
-- GenericTypeSort

-- kind TType
data TScalar
data TNewtype
data TRecord
data TSum
data TIndex
data TAt
data TTraversed

-- kind TList
data TCons (tt :: Type) tl
infixr 6 type TCons as :

class TTypeFamily t (tl :: Type) | t -> tl
instance ttypeFamilyUnit     :: TTypeFamily Unit         TScalar
instance ttypeFamilyInt      :: TTypeFamily Int          TScalar
instance ttypeFamilyNumber   :: TTypeFamily Number       TScalar
instance ttypeFamilyString   :: TTypeFamily String       TScalar
instance ttypeFamilyChar     :: TTypeFamily Char         TScalar
instance ttypeFamilyBoolean  :: TTypeFamily Boolean      TScalar
instance ttypeFamilyIdentity :: TTypeFamily (Identity a) TNewtype
instance ttypeFamilyRecord   :: TTypeFamily (Record a)   TRecord
instance ttypeFamilyMaybe    :: TTypeFamily (Maybe a)    (TSum : TTraversed)
instance ttypeFamilyArray    :: TTypeFamily (Array a)    (TIndex : TTraversed)
instance ttypeFamilySet      :: TTypeFamily (Set v)      (TIndex : TAt : TTraversed)
instance ttypeFamilyMap      :: TTypeFamily (Map k v)    (TIndex : TAt : TTraversed)

class CLenses' li <=
  GenericTypeSort li s i o | li s i -> o where
  genericTypeSort :: li s i -> o

instance genericTypeSortImpl ::
  ( TTypeFamily i tl
  , GenericTypeSortDispatch tl li s i o
  )
  => GenericTypeSort li s i o where
  genericTypeSort _i = genericTypeSortDispatch (Proxy :: Proxy tl) _i

-- GenericTypeSort > GenericTypeSortDispatch
class CLenses' li <=
  GenericTypeSortDispatch tl li s i o | tl li s i -> o where
  genericTypeSortDispatch :: Proxy tl -> li s i -> o

instance genericTypeSortDispatchTScalar ::
  CLenses' li
  => GenericTypeSortDispatch TScalar li s i (li s i) where
  genericTypeSortDispatch _ _i = _i
else
instance genericTypeSortDispatchTNewtype ::
  GenericNewtype li s i o
  => GenericTypeSortDispatch TNewtype li s i o where
  genericTypeSortDispatch _ = genericNewtype
else
instance genericTypeSortDispatchTList ::
  ( CLenses' li
  , GenericTypeListDispatch (tt : rest) li s i () o
  )
  => GenericTypeSortDispatch (tt : rest) li s i (Record o) where
  genericTypeSortDispatch tl _i = Builder.build (genericTypeListDispatch tl _i) {}
else
instance genericTypeSortDispatchSingle ::
  GenericTypeDispatch tt li s i () o
  => GenericTypeSortDispatch tt li s i (Record o) where
  genericTypeSortDispatch tt _i = Builder.build (genericTypeDispatch tt _i) {}

-- GenericTypeSort > GenericTypeSortDispatch > GenericTypeListDispatch
class CLenses' li <=
  GenericTypeListDispatch tl li s i (from :: # Type) (to :: # Type)
  | tl li s i from -> to where
  genericTypeListDispatch :: Proxy tl -> li s i -> Builder (Record from) (Record to)

instance genericTypeListDispatchCons ::
  ( GenericTypeDispatch a li s i to1 to
  , GenericTypeListDispatch b li s i from to1
  )
  => GenericTypeListDispatch (a : b) li s i from to where
  genericTypeListDispatch _ _i =
    genericTypeDispatch (Proxy :: Proxy a) _i
    <<< genericTypeListDispatch (Proxy :: Proxy b) _i
else
instance genericTypeListDispatchSingle ::
  GenericTypeDispatch a li s i from to
  => GenericTypeListDispatch a li s i from to where
  genericTypeListDispatch a _i = genericTypeDispatch a _i

-- GenericTypeSort > GenericTypeSortDispatch > GenericTypeDispatch
class CLenses' li <=
  GenericTypeDispatch (tt :: Type) li s i (from :: # Type) (to :: # Type)
  | tt li s i from -> to where
  genericTypeDispatch :: Proxy tt -> li s i -> Builder (Record from) (Record to)

instance genericTypeDispatchRecord ::
  GenericLensRecord li s i from to
  => GenericTypeDispatch TRecord li s (Record i) from to where
  genericTypeDispatch _ = genericLensRecord

instance genericTypeDispatchIndex ::
  GenericIndex li s i from to
  => GenericTypeDispatch TIndex li s i from to where
  genericTypeDispatch _ = genericIndex

instance genericTypeDispatchAt ::
  GenericAt li s i from to
  => GenericTypeDispatch TAt li s i from to where
  genericTypeDispatch _ = genericAt

instance genericTypeDispatchSum ::
  GenericPrismSum li s i from to
  => GenericTypeDispatch TSum li s i from to where
  genericTypeDispatch _ = genericPrismSum

instance genericTypeDispatchTraversed ::
  GenericTraversed li s f i from to
  => GenericTypeDispatch TTraversed li s (f i) from to where
  genericTypeDispatch _ = genericTraversed

