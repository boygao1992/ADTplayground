module Type.Path where

import Prelude
import Type.Prelude (class IsSymbol, Proxy(..))

import Prim.Row as Row
import Type.Data.List (type (:), kind List, LProxy(..))
import Type.Data.List as List
import Data.Generic.Rep (class Generic, Argument, Constructor)

class GetConstructorName rep (name :: Symbol) | rep -> name
instance getConstructorNameImpl :: GetConstructorName (Constructor label arg) label
class GetArgument rep arg | rep -> arg
instance getArgumentImpl :: GetArgument (Constructor label (Argument arg)) arg

data Edge (label :: Symbol)
data Path (path :: List {- Edge -}) i o = Path
data Query (paths :: Type {- Record -}) (i :: List) (o :: List) = Query

class IsEdge e
instance isEdgeEdge :: IsEdge (Edge label)

class IsEdgeList (l :: List)
instance isEdgeListBase :: IsEdgeList List.Nil
instance isEdgeListInduction ::
  ( IsEdge e
  , IsEdgeList es
  )
  => IsEdgeList (e : es)

class IsEdgeList es <=
  LiftEdgeList (es :: List) (t :: Type) o | es t -> o
instance liftEdgeListNil :: LiftEdgeList List.Nil t t
instance liftEdgeListCons ::
  ( LiftEdgeList es t o'
  , IsSymbol label
  , Row.Cons label o' () o
  )
  => LiftEdgeList (Edge label : es) t (Record o)

liftEdgeList :: forall es t o. LiftEdgeList es t o => LProxy es -> Proxy t -> Proxy o
liftEdgeList _ _ = Proxy :: Proxy o

testLiftEdgeList :: Proxy { x :: { y :: { z :: Int } } }
testLiftEdgeList = liftEdgeList
  (LProxy :: LProxy (Edge "x" : Edge "y" : Edge "z" : List.Nil))
  (Proxy :: Proxy Int)

class IsPath p
instance isPathPath :: IsPath (Path path i o)

class (IsPath p1, IsPath p2, IsPath p) <=
  PathCompose p1 p2 p | p1 p2 -> p
instance pathCompose ::
  ( IsEdgeList p1
  , IsEdgeList p2
  , List.Append p1 p2 p
  )
  => PathCompose (Path p1 x o) (Path p2 i x) (Path p i o)

class IsEdgeList es <=
  PathVerify (es :: List {- Edge -}) i o
instance pathVerifyNil :: PathVerify List.Nil o o
instance pathVerifyCons ::
  ( Generic i iRep
  , GetArgument iRep (Record row)
  , Row.Cons label i' restRow row
  , PathVerify es i' o
  )
  => PathVerify ((Edge label) : es) i o

pathVerify :: forall es i o. PathVerify es i o => LProxy es -> Proxy i -> Proxy o -> Unit
pathVerify _ _ _ = unit

testPathVerify :: Unit
testPathVerify = pathVerify
  (LProxy :: LProxy (Edge "friend" : Edge "friend" :Edge "name" : List.Nil))
  (Proxy :: Proxy Person) (Proxy :: Proxy String)

class IsPath p <= QueryLiftPath p q | p -> q
instance queryLiftPathImpl ::
  ( IsEdgeList es
  , PathVerify es i o
  , LiftEdgeList es o paths'
  , List.Singleton i i'
  , List.Singleton o o'
  , Generic i iRep
  , GetConstructorName iRep name
  , IsSymbol name
  , Row.Cons name paths' () paths
  )
  => QueryLiftPath (Path es i o) (Query (Record paths) i' o')

liftPath :: forall p q. QueryLiftPath p q => Proxy p -> Proxy q
liftPath _ = Proxy :: Proxy q

testLiftPath :: Proxy
  (Query
     { "Person" :: { friend :: { friend :: { name :: String } } }
     }
     (Person : List.Nil)
     (String : List.Nil)
  )
testLiftPath = liftPath (Proxy :: Proxy (Path (Edge "friend" : Edge "friend" : Edge "name" : List.Nil) Person String))

-- TODO QueryBuilder

-- TODO GroupPath (paths :: List {- Path -})
-- (grouped :: List {- SProxy /\ List {- Path -} -})

newtype Person = Person
  { name :: String
  , friend :: Person
  }
derive instance genericPerson :: Generic Person _
