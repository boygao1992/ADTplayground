module Data.Path where

import Prelude

import Data.Exists1 (Exists, mkExists)
import Data.Leibniz (type (~))
import Data.Map (Map)
import Data.Map as Map
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.Tuple
import Data.Tuple.Nested


data Path i o
  = Path
  | PathId (i ~ o)
  | PathCompose (Exists (PathComposeF i o))
data PathComposeF i o x = PathComposeF (Path x o) (Path i x)
path = Path :: forall i o. Path i o
pathId = PathId identity :: forall i. Path i i
pathCompose :: forall i o x. Path x o -> Path i x -> Path i o
pathCompose xo ix = PathCompose $ mkExists $ PathComposeF xo ix

instance semigroupoidPath :: Semigroupoid Path where
  compose = pathCompose
instance categoryPath :: Category Path where
  identity = pathId

newtype Edge = Edge String
derive newtype instance eqEdge :: Eq Edge
derive newtype instance ordEdge :: Ord Edge

data Constant
  = CString String
  | CInt Int
  | CNumber Number
  | CBool Boolean

data Lit
  = Var String
  | Constant Constant

data Pred
  = Eq Lit
  | Gt Lit
  | Lt Lit
  | In (Array Constant)
  | Not Pred
  | And Pred Pred
  | Or Pred Pred

newtype Conditions = Conditions (Object Pred)

data Field
  = Node
  | Branch Paths
  | Filtered Conditions Paths
  | Let String Field

type Paths = Object Field

sample :: Field
sample =
  Branch $ Object.fromHomogeneous
  { "Person":
      Let "wenbo"
      $ Filtered
        ( Conditions $ Object.fromHomogeneous
          { name: Eq $ Constant $ CString "wenbo" })
      $ Object.fromHomogeneous
      { friend:
          Let "friend"
          $ Filtered
            ( Conditions $ Object.fromHomogeneous
              { age: Gt $ Var "wenbo" })
          $ Object.fromHomogeneous
          { age: Let "friend's age" Node
          , post: Branch $ Object.fromHomogeneous
              { id: Node
              , content: Node
              , comment:
                  Filtered
                    ( Conditions $ Object.fromHomogeneous
                      { date: Gt (Var "friend's age") })
                  $ Object.fromHomogeneous
                  { content: Node }
              }
          }
      }
  }
