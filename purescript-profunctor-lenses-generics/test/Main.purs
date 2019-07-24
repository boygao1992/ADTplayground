module Test.Main where

import Prelude

import Control.Apply (lift2)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.Generic.Rep.Lens (class TTypeFamily, TSum, _Argument, _Constructor, _Ctor', _Generic', _ProductFirst, _ProductSecond, _SumInl, _SumInr, genericLens)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (class Wander, Optic', Prism', Traversal', wander, (.~), (^?))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Prim.RowList (kind RowList)
import Record as Record
import Record.Builder as Builder
import Type.Prelude (SProxy(..))
import Type.Proxy (Proxy(..))

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
instance ttypeFamilyA :: TTypeFamily A TSum
instance showA :: Show A where show = genericShow
data B = B
derive instance genericB :: Generic B _
instance ttypeFamilyB :: TTypeFamily B TSum
instance showB :: Show B where show = genericShow
data C = C
derive instance genericC :: Generic C _
instance ttypeFamilyC :: TTypeFamily C TSum
instance showC :: Show C where show = genericShow

data TestSum
  = Empty
  | One A
  | Two A B
  | Three A B C -- Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
  | TestRecord { x :: { y :: { z :: Maybe A } } }
  | TestMap (Map String { a :: A })
derive instance genericTestSum :: Generic TestSum _
instance ttypeFamilyTestSum :: TTypeFamily TestSum TSum

_ThreeA :: Traversal' TestSum A
_ThreeA = _Generic' <<< _SumInr <<< _SumInr <<< _SumInr <<< _SumInl <<< _Constructor <<< _ProductFirst <<< _Argument

_ThreeB :: Traversal' TestSum B
_ThreeB = _Generic' <<< _SumInr <<< _SumInr <<< _SumInr <<< _SumInl <<< _Constructor <<< _ProductSecond <<< _ProductFirst <<< _Argument

_ThreeC :: Traversal' TestSum C
_ThreeC = _Generic' <<< _SumInr <<< _SumInr <<< _SumInr <<< _SumInl <<< _Constructor <<< _ProductSecond <<< _ProductSecond <<< _Argument

-- DONE type
type ThreeType =
  { _1 :: A
  , _2 :: B
  , _3 :: C
  }

-- DONE Lenses
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

-- DONE Traversal
_Three :: forall p. Wander p => Optic' p TestSum ThreeType -- Optic' p s (Record a)
_Three = wander _ThreeMerge

-- DONE Merge
_ThreeMerge
  :: forall f
  . Applicative f
  => (ThreeType -> f ThreeType) -- Record a -> f (Record a)
  -> TestSum -- s
  -> f TestSum -- f s
_ThreeMerge coalg s =
  fromMaybe (pure s)
  $ map (map $ _ThreeSet s)
  $ map coalg
  $ _ThreeGet s

-- DONE Set
_ThreeSet
  :: TestSum -- s
  -> ThreeType -- Record a
  -> TestSum -- s
_ThreeSet s a
  = s
    # (Record.get (SProxy :: SProxy "_1") _ThreeLenses)
      .~ (Record.get (SProxy :: SProxy "_1") a)
    # (Record.get (SProxy :: SProxy "_2") _ThreeLenses)
      .~ (Record.get (SProxy :: SProxy "_2") a)
    # (Record.get (SProxy :: SProxy "_3") _ThreeLenses)
      .~ (Record.get (SProxy :: SProxy "_3") a)

-- DONE Get
_ThreeGet
  :: TestSum
  -> Maybe ThreeType
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
  $ lift2 (<<<) ( Builder.insert (SProxy :: SProxy "_1")
      <$> s ^? (Record.get (SProxy :: SProxy "_1") _ThreeLenses)
    )
  $ Just identity

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
testsumLenses :: forall p. Wander p =>
  { _Empty_ :: Optic' p TestSum Unit
  , _One :: { _A_ :: Optic' p TestSum Unit }
  , _One_ :: Optic' p TestSum A
  , _Two ::
      { _1 :: { _A_ :: Optic' p TestSum Unit }
      , _1_ :: Optic' p TestSum A
      , _2 :: { _B_ :: Optic' p TestSum Unit }
      , _2_ :: Optic' p TestSum B
      }
  , _Two_ :: Optic' p TestSum { _1 :: A, _2 :: B }
  , _Three ::
      { _1 :: { _A_ :: Optic' p TestSum Unit }
      , _1_ :: Optic' p TestSum A
      , _2 :: { _B_ :: Optic' p TestSum Unit }
      , _2_ :: Optic' p TestSum B
      , _3 :: { _C_ :: Optic' p TestSum Unit }
      , _3_ :: Optic' p TestSum C
      }
  , _Three_ :: Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
  , _TestRecord ::
      { x :: { y :: { z :: { _Just :: { _A_ :: Optic' p TestSum Unit }
                        , _Just_ :: Optic' p TestSum A
                        , _Nothing_ :: Optic' p TestSum Unit
                        }
                  , z_ :: Optic' p TestSum (Maybe A)
                  }
            , y_ :: Optic' p TestSum { z :: Maybe A }
            }
      , x_ :: Optic' p TestSum { y :: { z :: Maybe A } }
      }
  , _TestRecord_ :: Optic' p TestSum { x :: { y :: { z :: Maybe A } } }
  , _TestMap :: String ->
      { ix ::
          { a :: { _A_ :: Optic' p TestSum Unit }
          , a_ :: Optic' p TestSum A
          }
      , ix_ :: Optic' p TestSum { a :: A }
      , at ::
          { _Just :: { a :: { _A_ :: Optic' p TestSum Unit }
                    , a_ :: Optic' p TestSum A
                    }
          , _Just_ :: Optic' p TestSum { a :: A }
          , _Nothing_ :: Optic' p TestSum Unit
          }
      , at_ :: Optic' p TestSum (Maybe { a :: A })
      }
  , _TestMap_ :: Optic' p TestSum (Map String { a :: A })
  }
testsumLenses = genericLens (Proxy :: Proxy TestSum)

_TestSumThreeC :: Traversal' TestSum C
_TestSumThreeC = testsumLenses._Three._3_

_TestSumTestMapAt :: String -> Traversal' TestSum (Maybe { a :: A })
_TestSumTestMapAt key = testsumLenses._TestMap key # _.at_

_TestSumTestMapIxA :: String -> Traversal' TestSum A
_TestSumTestMapIxA key = testsumLenses._TestMap key # _.ix.a_

_TestSumTestRecordXYZJust :: Traversal' TestSum A
_TestSumTestRecordXYZJust = testsumLenses._TestRecord.x.y.z._Just_

-- test _Ctor'

_OneCtor :: Prism' TestSum A
_OneCtor = _Ctor' (SProxy :: SProxy "One")

_ThreeCtor :: Traversal' TestSum { _1 :: A, _2 :: B, _3 :: C }
_ThreeCtor = _Ctor' (SProxy :: SProxy "Three")


main :: Effect Unit
main = do
  log "You should add some tests."
