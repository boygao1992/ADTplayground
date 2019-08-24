module Test.CLenses where

import Prelude
import Type.Prelude (SProxy(..))

import Prim.RowList (kind RowList)
import Type.Proxy (Proxy(..))

import Control.Apply (lift2)
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.CLens.Simple (class TTypeFamily, TNewtype, TSum, _Argument', _Constructor', _Ctor', _Generic', _Product'First', _Product'Second', _Sum'Inl', _Sum'Inr', genericLens)
import Data.Generic.Rep.Lens.Constraints.Simple (CPrism', CTraversal'(..), (..~), (.<<<), (.^?))
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (wander)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Record as Record
import Record.Builder as Builder

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

newtype SimpleNewtype = SimpleNewtype Unit
derive instance newtypeSimpleNewtype :: Newtype SimpleNewtype _
instance ttypeFamilySimpleNewtype :: TTypeFamily SimpleNewtype TNewtype

data TestSum
  = Empty
  | One A
  | Two A B
  | Three A B C -- Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
  | TestRecord { x :: { y :: { z :: Maybe A } } }
  | TestMap (Map String { a :: A })
  | TestNewtype SimpleNewtype
derive instance genericTestSum :: Generic TestSum _
instance ttypeFamilyTestSum :: TTypeFamily TestSum TSum

_CThreeA :: CTraversal' TestSum A
_CThreeA = _Generic' .<<< _Sum'Inr' .<<< _Sum'Inr' .<<< _Sum'Inr' .<<< _Sum'Inl' .<<< _Constructor' .<<< _Product'First' .<<< _Argument'

_CThreeB :: CTraversal' TestSum B
_CThreeB = _Generic' .<<< _Sum'Inr' .<<< _Sum'Inr' .<<< _Sum'Inr' .<<< _Sum'Inl' .<<< _Constructor' .<<< _Product'Second' .<<< _Product'First' .<<< _Argument'

_CThreeC :: CTraversal' TestSum C
_CThreeC = _Generic' .<<< _Sum'Inr' .<<< _Sum'Inr' .<<< _Sum'Inr' .<<< _Sum'Inl' .<<< _Constructor' .<<< _Product'Second' .<<< _Product'Second' .<<< _Argument'

-- DONE type
type ThreeType =
  { _1 :: A
  , _2 :: B
  , _3 :: C
  }

-- DONE Lenses
_ThreeLenses
  :: { _1 :: CTraversal' TestSum A
    , _2 :: CTraversal' TestSum B
    , _3 :: CTraversal' TestSum C
    }
_ThreeLenses =
  { _1: _CThreeA
  , _2: _CThreeB
  , _3: _CThreeC
  }

-- -- DONE Traversal
_Three :: CTraversal' TestSum ThreeType -- Optic' p s (Record a)
_Three = CTraversal' (wander _ThreeMerge)

-- -- DONE Merge
_ThreeMerge
  :: forall f
  . Applicative f
  => (ThreeType -> f ThreeType) -- Record a -> f (Record a)
  -> TestSum -> f TestSum -- s -> f s
_ThreeMerge coalg s =
  fromMaybe (pure s)
  $ map (map $ _ThreeSet s)
  $ map coalg
  $ _ThreeGet s

-- -- DONE Set
_ThreeSet
  :: TestSum -- s
  -> ThreeType -- Record a
  -> TestSum -- s
_ThreeSet s a
  = s
    # (Record.get (SProxy :: SProxy "_1") _ThreeLenses)
      ..~ (Record.get (SProxy :: SProxy "_1") a)
    # (Record.get (SProxy :: SProxy "_2") _ThreeLenses)
      ..~ (Record.get (SProxy :: SProxy "_2") a)
    # (Record.get (SProxy :: SProxy "_3") _ThreeLenses)
      ..~ (Record.get (SProxy :: SProxy "_3") a)

-- DONE Get
_ThreeGet
  :: TestSum -- s
  -> Maybe ThreeType -- Maybe (Record a)
_ThreeGet s
  = map (Builder.build <@> {})
  $ lift2 (<<<)
    ( Builder.insert (SProxy :: SProxy "_3")
      <$> s .^? (Record.get (SProxy :: SProxy "_3") _ThreeLenses)
    )
  $ lift2 (<<<)
    ( Builder.insert (SProxy :: SProxy "_2")
      <$> s .^? (Record.get (SProxy :: SProxy "_2") _ThreeLenses)
    )
  $ lift2 (<<<) ( Builder.insert (SProxy :: SProxy "_1")
      <$> s .^? (Record.get (SProxy :: SProxy "_1") _ThreeLenses)
    )
  $ Just identity

-- test _Ctor'

_OneCtor :: CPrism' TestSum A
_OneCtor = _Ctor' (Proxy :: Proxy TestSum) (SProxy :: SProxy "One")

_ThreeCtor :: CTraversal' TestSum
  { _1 :: A
  , _2 :: B
  , _3 :: C
  }
_ThreeCtor = _Ctor' (Proxy :: Proxy TestSum) (SProxy :: SProxy "Three")

-- test genericLens

testsumLenses ::
  { _Empty_ :: CPrism' TestSum Unit
  , _One :: { _A_ :: CPrism' TestSum Unit }
  , _One_ :: CPrism' TestSum A
  , _Two ::
       { _1 :: { _A_ :: CTraversal' TestSum Unit }
       , _1_ :: CTraversal' TestSum A
       , _2 :: { _B_ :: CTraversal' TestSum Unit }
       , _2_ :: CTraversal' TestSum B
       }
  , _Two_ :: CTraversal' TestSum { _1 :: A, _2 :: B }
  , _Three ::
       { _1 :: { _A_ :: CTraversal' TestSum Unit }
       , _1_ :: CTraversal' TestSum A
       , _2 :: { _B_ :: CTraversal' TestSum Unit }
       , _2_ :: CTraversal' TestSum B
       , _3 :: { _C_ :: CTraversal' TestSum Unit }
       , _3_ :: CTraversal' TestSum C
       }
  , _Three_ :: CTraversal' TestSum { _1 :: A, _2 :: B, _3 :: C }
  , _TestRecord ::
       { x :: { y :: { z :: { _Just :: { _A_ :: CTraversal' TestSum Unit }
                         , _Just_ :: CTraversal' TestSum A
                         , _Nothing_ :: CTraversal' TestSum Unit
                         , traversed :: { _A_ :: CTraversal' TestSum Unit }
                         , traversed_ :: CTraversal' TestSum A
                         }
                   , z_ :: CTraversal' TestSum (Maybe A)
                   }
             , y_ :: CTraversal' TestSum { z :: Maybe A }
             }
       , x_ :: CTraversal' TestSum { y :: { z :: Maybe A } }
       }
  , _TestRecord_ :: CPrism' TestSum { x :: { y :: { z :: Maybe A } } }
  , _TestMap ::
      { at :: String ->
          { _Just ::
              { a :: { _A_ :: CTraversal' TestSum Unit }
              , a_ :: CTraversal' TestSum A
              }
          , _Just_ :: CTraversal' TestSum { a :: A }
          , _Nothing_ :: CTraversal' TestSum Unit
          , traversed ::
              { a :: { _A_ :: CTraversal' TestSum Unit }
              , a_ :: CTraversal' TestSum A
              }
              , traversed_ :: CTraversal' TestSum { a :: A }
          }
      , at_ :: String -> CTraversal' TestSum (Maybe { a :: A })
      , ix :: String ->
          { a :: { _A_ :: CTraversal' TestSum Unit }
          , a_ :: CTraversal' TestSum A
          }
      , ix_ :: String -> CTraversal' TestSum { a :: A }
      , traversed ::
          { a :: { _A_ :: CTraversal' TestSum Unit }
          , a_ :: CTraversal' TestSum A
          }
      , traversed_ :: CTraversal' TestSum { a :: A }
      }
  , _TestMap_ :: CPrism' TestSum (Map String { a :: A })
  , _TestNewtype :: CPrism' TestSum Unit
  , _TestNewtype_ :: CPrism' TestSum SimpleNewtype
  }
testsumLenses = genericLens (Proxy :: Proxy TestSum)

_TestSumThreeC :: CTraversal' TestSum C
_TestSumThreeC = testsumLenses._Three._3_

_TestSumTestMapAt :: String -> CTraversal' TestSum (Maybe { a :: A })
_TestSumTestMapAt key = testsumLenses._TestMap.at_ key

_TestSumTestMapIxA :: String -> CTraversal' TestSum A
_TestSumTestMapIxA key = testsumLenses._TestMap.ix key # _.a_

_TestSumTestRecordXYZJust :: CTraversal' TestSum A
_TestSumTestRecordXYZJust = testsumLenses._TestRecord.x.y.z._Just_
