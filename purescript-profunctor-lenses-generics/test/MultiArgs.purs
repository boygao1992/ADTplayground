module Test.MultiArgs where

import Prelude

import Data.Generic.Rep.CLens
import Data.Generic.Rep.Lens.Constraints
import Control.Apply (lift2)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
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


data A = A
derive instance genericA :: Generic A _
-- instance ttypeFamilyA :: TTypeFamily A TSum
instance showA :: Show A where show = genericShow
data B = B
derive instance genericB :: Generic B _
-- instance ttypeFamilyB :: TTypeFamily B TSum
instance showB :: Show B where show = genericShow
data C = C
derive instance genericC :: Generic C _
-- instance ttypeFamilyC :: TTypeFamily C TSum
instance showC :: Show C where show = genericShow

data TestSum
  = Empty
  | One A
  | Two A B
  | Three A B C -- Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
  | TestRecord { x :: { y :: { z :: Maybe A } } }
  | TestMap (Map String { a :: A })
derive instance genericTestSum :: Generic TestSum _
-- instance ttypeFamilyTestSum :: TTypeFamily TestSum TSum

_CThreeA :: CTraversal TestSum TestSum A A
_CThreeA = _Generic' .<<< _SumInr .<<< _SumInr .<<< _SumInr .<<< _SumInl .<<< _Constructor .<<< _ProductFirst .<<< _Argument

_CThreeB :: CTraversal TestSum TestSum B B
_CThreeB = _Generic' .<<< _SumInr .<<< _SumInr .<<< _SumInr .<<< _SumInl .<<< _Constructor .<<< _ProductSecond .<<< _ProductFirst .<<< _Argument

_CThreeC :: CTraversal TestSum TestSum C C
_CThreeC = _Generic' .<<< _SumInr .<<< _SumInr .<<< _SumInr .<<< _SumInl .<<< _Constructor .<<< _ProductSecond .<<< _ProductSecond .<<< _Argument

-- DONE type
type ThreeType =
  { _1 :: A
  , _2 :: B
  , _3 :: C
  }

-- DONE Lenses
_ThreeLenses
  :: { _1 :: CTraversal TestSum TestSum A A
    , _2 :: CTraversal TestSum TestSum B B
    , _3 :: CTraversal TestSum TestSum C C
    }
_ThreeLenses =
  { _1: _CThreeA
  , _2: _CThreeB
  , _3: _CThreeC
  }

-- -- DONE Traversal
_Three :: CTraversal TestSum TestSum ThreeType ThreeType -- Optic' p s (Record a)
_Three = CTraversal (wander _ThreeMerge)

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

_OneCtor :: CPrism TestSum TestSum A A
_OneCtor = _Ctor' (Proxy :: Proxy TestSum) (SProxy :: SProxy "One")

_ThreeCtor :: CTraversal TestSum TestSum
  { _1 :: A
  , _2 :: B
  , _3 :: C
  }
  { _1 :: A
  , _2 :: B
  , _3 :: C
  }
_ThreeCtor = _Ctor' (Proxy :: Proxy TestSum) (SProxy :: SProxy "Three")

