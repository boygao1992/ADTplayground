module Data.Path where

import Prelude

import Data.Exists1 (Exists, mkExists)
import Data.Leibniz (type (~))
import Data.Map (Map)
import Data.Map as Map
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

data TypeRep
  = TString
  | TInt
  | TNumber
  | TBool

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

data Logic
  = LiftPred Pred
  | Not Logic
  | And Logic Logic
  | Or Logic Logic

newtype Conditions = Conditions (Map Edge Logic)

data Target
  = Field Field
  | Let String Field
  | Filtered Conditions Branch
  | LetFiltered String Conditions Branch

data Field
  = Branch Branch
  | Node TypeRep

type Branch = Map Edge Target

sample :: Field
sample =
  Branch $ Map.fromFoldable
  [ Tuple (Edge "Person")
    $ LetFiltered "wenbo"
      ( Conditions $ Map.fromFoldable
        [ Edge "name" /\ LiftPred (Eq $ Constant $ CString "wenbo")])
    $ Map.fromFoldable
      [ Tuple (Edge "friend")
        $ LetFiltered "friend"
          ( Conditions $ Map.fromFoldable
            [ Edge "age" /\ LiftPred (Gt $ Var "wenbo")])
        $ Map.fromFoldable
          [ Edge "age" /\ (Let "friend's age" (Node TInt))
          , Tuple (Edge "post") $ Field $ Branch $ Map.fromFoldable
              [ Edge "id" /\ Field (Node TInt)
              , Edge "content" /\ Field (Node TString)
              , Tuple (Edge "comment")
                  $ Filtered
                    ( Conditions $ Map.fromFoldable
                      [ Edge "date" /\ LiftPred (Gt (Var "friend's age"))])
                  $ Map.fromFoldable
                  [ Edge "content" /\ Field (Node TString)]
              ]
          ]
      ]
  ]
