module Type.Path where

import Prelude
import Type.Prelude

import Data.Generic.Rep.Type.Utils (class GetArgument, class GetConstructorName)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Type.Data.Boolean as B
import Type.Data.List (type (:), kind List, LProxy(..))
import Type.Data.List as List
import Type.Data.Record

import Data.Generic.Rep (class Generic, Argument, Constructor)
import Data.Tuple.Nested

type SampleQuerySpec =
  Query (Person : Post : Comment : List.Nil)
  { "Person" :: Let "wenbo" (Filtered { name :: Eq String }
      { friend :: Let "friend" (Filtered { age :: Gt (Var "wenbo") }
          { age :: Let "friend's age" Int
          , post ::
              { id :: Int
              , content :: String
              , comment :: Filtered { date :: Gt (Var "friend's age")}
                  { content :: String
                  }
              }
          })
      })
  }
data Executable result
type GeneratedQueryTemplate
  = String
  -> Executable
      { "Person" ::
          { friend ::
              { age :: Int
              , post ::
                  { id :: Int
                  , content :: String
                  , comment :: { content :: String }
                  }
              }
          }
      }

-----------
-- Constant

class IsConstant constant
instance isConstantString :: IsConstant (CString str)
instance isConstantInt :: IsConstant (CInt int) -- TODO class IsInt
instance isConstantBool :: IsConstant (CBool bool)

data CString (str :: Symbol)
data CInt (int :: Symbol)
data CBool (bool :: B.Boolean)

class IsConstantList (cs :: List {- IsConstant -})
instance isConstantListNil :: IsConstantList List.Nil
instance isConstantListCons ::
  ( IsConstant c
  , IsConstantList rest
  )
  => IsConstantList (List.Cons c rest)

------
-- Lit

class IsLit lit
instance isLitVar :: IsLit (Var label)
else
instance isLitConstant :: IsConstant constant => IsLit constant

data Var (label :: Symbol)
-- data Constant constant -- NOTE IsConstant

-------
-- Pred

class IsPred pred
instance isPredEq :: IsLit lit => IsPred (Eq lit)
instance isPredGt :: IsLit lit => IsPred (Gt lit)
instance isPredLt :: IsLit lit => IsPred (Lt lit)
instance isPredIn :: IsConstantList cs => IsPred (In cs)
instance isPredNOT :: IsPred pred => IsPred (NOT pred)
instance isPredAND :: (IsPred pred1, IsPred pred2) => IsPred (AND pred1 pred2)
instance isPredOR :: (IsPred pred1, IsPred pred2) => IsPred (OR pred1 pred2)

data Eq lit
data Gt lit
data Lt lit
data In (consts :: List)
data NOT pred
data AND pred1 pred2
data OR pred1 pred2

--------
-- Field

class IsField field
instance isFieldNode :: IsField Node
instance isFieldBranch :: IsPaths pathsRow => IsField (Record pathsRow)
instance isFieldFiltered :: IsPaths paths => IsField (Filtered conditions (Record paths))
instance isFieldLet :: IsField field => IsField (Let var field)

data Node
-- data Branch paths -- NOTE Record
data Filtered conditions paths
data Let (var :: Symbol) field

--------
-- Paths

class IsPaths (pathsRow :: # Type)
instance isPathsRL ::
  ( RL.RowToList pathsRow pathsRL
  , IsPathsRL pathsRL
  )
  => IsPaths pathsRow

class IsPathsRL (pathsRL :: RowList)
instance isPathsRLNil :: IsPathsRL RL.Nil
instance isPathsRLCons ::
  ( IsField field
  , IsPathsRL rest
  )
  => IsPathsRL (RL.Cons label field rest)

data Edge (label :: Symbol)
data Path (path :: List {- Edge -}) i o = Path
data Query (ts :: List {- Type -}) (paths :: Type {- Paths -}) = Query

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

class IsQuery q
instance isQueryQuery :: IsRecord paths => IsQuery (Query ts paths)

class (IsPath p, IsQuery q) <= QueryLiftPath p q | p -> q
instance queryLiftPathImpl ::
  ( IsEdgeList es
  , PathVerify es i o
  , LiftEdgeList es o paths
  , Generic i iRep
  , GetConstructorName iRep name
  , IsSymbol name
  , Row.Cons name paths () query
  , List.Singleton i i'
  )
  => QueryLiftPath (Path es i o) (Query i' (Record query))

liftPath :: forall p q. QueryLiftPath p q => Proxy p -> Proxy q
liftPath _ = Proxy :: Proxy q

testLiftPath :: Proxy
  (Query
    (Person : List.Nil)
     { "Person" :: { friend :: { friend :: { name :: String } } } }
  )
testLiftPath = liftPath (Proxy :: Proxy (Path (Edge "friend" : Edge "friend" : Edge "name" : List.Nil) Person String))

class IsRecord paths <=
  QueryVerify (ts :: List {- Type -}) (paths :: Type {- Record Field -})

instance queryVerifyRoot ::
  ( IsPaths pathsRow
  , RL.RowToList pathsRow pathsRL
  , QueryVerifyRoot ts pathsRL
  )
  => QueryVerify ts (Record pathsRow)

class QueryVerifyRoot (ts :: List) (pathsRL :: RowList)
instance queryVerifyRootNil :: QueryVerifyRoot ts RL.Nil
instance queryVerifyRootCons ::
  ( List.GetByName ts label t
  , QueryVerifyPaths t paths
  , QueryVerifyRoot ts restPaths
  )
  => QueryVerifyRoot ts (RL.Cons label paths restPaths)

class IsField field <= QueryVerifyPaths t field
instance queryVerifyPathsBase :: QueryVerifyPaths t Node
instance queryVerifyPathsBranch ::
  ( IsPaths pathsRow
  , RL.RowToList pathsRow pathsRL
  , Generic t tRep
  , GetArgument tRep (Record tRow)
  , QueryVerifyPathsRL tRow pathsRL
  )
  => QueryVerifyPaths t (Record pathsRow)
instance queryVerifyPathsFiltered ::
  ( IsPaths pathsRow
  -- TODO verify conditions
  , QueryVerifyPaths t (Record pathsRow)
  )
  => QueryVerifyPaths t (Filtered conditions (Record pathsRow))
instance queryVerifyPathsLet ::
  QueryVerifyPaths t field
  => QueryVerifyPaths t (Let var field)

class QueryVerifyPathsRL (tRow :: # Type) (pathsRL :: RowList)

instance queryVerifyPathsRLNil :: QueryVerifyPathsRL t RL.Nil
instance queryVerifyPathsRLCons ::
  ( Row.Cons label t' restTRow tRow
  , QueryVerifyPaths t' paths
  , QueryVerifyPathsRL tRow restPaths
  )
  => QueryVerifyPathsRL tRow (RL.Cons label paths restPaths)

queryVerify :: forall ts paths. QueryVerify ts paths => LProxy ts -> Proxy paths -> Unit
queryVerify _ _ = unit

testQueryVerify :: Unit
testQueryVerify = queryVerify
  (LProxy :: LProxy (Person : Post : Comment : List.Nil))
  (Proxy :: Proxy
            { "Person" :: { friend :: { post :: { comment ::
                                                { id :: Node
                                                , content :: Node
                                                }}}
                , post ::
                    { id :: Node
                    , content :: Node
                    }
                }
            , "Post" :: { comment :: { from :: { author :: { name :: Node }}}}
            })

-- TODO QueryBuilder

-- TODO GroupPath (paths :: List {- Path -})
-- (grouped :: List {- SProxy /\ List {- Path -} -})

newtype Person = Person
  { id :: Int
  , name :: String
  , age :: Int
  , friend :: Person
  , post :: Post
  }
derive instance genericPerson :: Generic Person _

newtype Post = Post
  { id :: Int
  , content :: String
  , author :: Person
  , comment :: Comment
  }
derive instance genericPost :: Generic Post _

newtype Comment = Comment
  { id :: Int
  , content :: String
  , date :: Int
  , from :: Post
  }
derive instance genericComment :: Generic Comment _
