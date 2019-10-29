module Main where


import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Reader (Reader, asks)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (Object)
import Data.Functor.Mu (Mu)
import Matryoshka (class Corecursive, class Recursive, embed, project)

------------
-- Recursive
------------

-- class Functor f <= Recursive t f | t → f where
--   project :: t → f t

-- instance recursiveMu :: Functor f => Recursive (Mu f) f where
--   project = un In

instance recursiveList :: Recursive (List x) (ListF x) where
  project :: List x -> ListF x (List x)
  project =    -- ListF x (List x)
    map List   -- ListF x (Mu (ListF x))
    <<< project -- Mu (ListF x)
    <<< un List -- List x

instance recursiveExprTree :: Recursive (ExprTree x) (ExprTreeF x) where
  project = map ExprTree <<< project <<< un ExprTree

--------------
-- Corecursive
--------------

-- class Functor f <= Corecursive t f | t → f where
--   embed :: f t → t

-- instance corecursiveMu :: Functor f => Corecursive (Mu f) f where
--   embed = In

instance corecursiveList :: Corecursive (List x) (ListF x) where
  embed :: ListF x (List x) -> List x
  embed =            -- List x
    List             -- Mu (ListF x)
    <<< embed         -- ListF x (Mu (ListF x))
    <<< map (un List) -- ListF x (List x)

instance corecursiveExprTree :: Corecursive (ExprTree x) (ExprTreeF x) where
  embed = ExprTree <<< embed <<< map (un ExprTree)

-----
-- Mu
-----

-- newtype Mu f = In (f (Mu f))
-- derive instance newtypeMu :: Newtype (Mu f) _
-- derive instance eqMu :: Eq1 f => Eq (Mu f)

-------------------
-- Recursion Scheme
-------------------

type Algebra f a = f a -> a
type AlgebraM m f a = f a -> m a

type Coalgebra f a = a -> f a
type CoalgebraM m f a = a -> m (f a)

cata ∷ forall t f a. Recursive t f => Algebra f a → t → a
cata alg = alg <<< map (cata alg) <<< project

cataM'
  :: forall t f m a
  . Recursive t f
  => Monad m -- distributive
  => Traversable f
  => (f t -> m (f t))
  -> AlgebraM m f a -- f a -> m a
  -> t
  -> m a
cataM' pre algM = -- m a
  bindFlipped algM -- m (f a)
  <<< bindFlipped ( -- m (f a)
      traverse ( -- m a
         cataM' pre algM -- t
      ) -- f t
    ) -- m (f t)
  <<< pre -- f t
  <<< project -- t

cataM
  ∷ ∀ t f m a
  . Recursive t f
  ⇒ Monad m
  ⇒ Traversable f
  ⇒ AlgebraM m f a
  → t
  → m a
cataM algM = algM <=< traverse (cataM algM) <<< project

ana ∷ forall t f a. Corecursive t f => Coalgebra f a → a → t
ana coalg = embed <<< map (ana coalg) <<< coalg

anaM
  ∷ ∀ t f m a
  . Corecursive t f
  ⇒ Monad m
  ⇒ Traversable f
  ⇒ CoalgebraM m f a
  → a
  → m t
anaM coalgM = map embed <<< traverse (anaM coalgM) <=< coalgM

zipo ::
  forall f g t u c.
  Recursive t f =>
  Recursive u g =>
  (f (u -> c) -> g u -> c) ->
  t -> u -> c
zipo alg = cata (\x -> alg x <<< project)


-------
-- List
-------

newtype List x = List (Mu (ListF x))
data ListF x a
  = Nil
  | Cons x a
derive instance newtypeList :: Newtype (List x) _
derive instance functorListF :: Functor (ListF x)

-----------
-- ExprTree
-----------

type Key = String
data Expr = Expr

evalExpr :: Map Key ExprType -> Expr -> Either String ExprType
evalExpr _ _ = Left ""

newtype ExprTree x = ExprTree (Mu (ExprTreeF x))
data ExprTreeF x a
  = Pure x
  | Par (Object a)
  | Seq (Array a)
  | Keyed (Array (String /\ a))
  | Alt String a

data ExprNode
  = NExpr ExprStatus
  | NInput
  | NErrors

data ExprStatus
  = Unevaled Expr
  | Evaled ExprType
  | EvalFailed String

data Path
  = PathNil
  | PathPar String Path
  | PathSeq Int Path
  | PathKeyed String Path
  | PathAlt String Path

derive instance eqExpr :: Eq Expr

derive instance newtypeExprTree :: Newtype (ExprTree x) _
derive instance eqExprTree :: Eq x => Eq (ExprTree x)

derive instance eqExprTreeF :: (Eq x, Eq a) => Eq (ExprTreeF x a)
derive instance eq1ExprTreeF :: Eq x => Eq1 (ExprTreeF x)
derive instance functorExprTreeF :: Functor (ExprTreeF x)
instance foldableExprTreeF :: Foldable (ExprTreeF x) where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    Pure _ -> mempty
    Par os -> foldMap f os
    Seq as -> foldMap f as
    Keyed ts -> foldMap (\(_ /\ x) -> f x) ts
    Alt _ x -> f x
instance traversableExprTreeF :: Traversable (ExprTreeF x) where
  sequence = traverse identity
  traverse f = case _ of
    Pure x -> pure (Pure x)
    Par os -> Par <$> traverse f os
    Seq as -> Seq <$> traverse f as
    Keyed ts -> Keyed <$> traverse (\(key /\ x) -> (key /\ _) <$> f x) ts
    Alt key x -> Alt key <$> f x

derive instance eqExprNode :: Eq ExprNode

derive instance eqExprStatus :: Eq ExprStatus

derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path

type VarStore = Map Path VarMap
type VarMap = Map Key ExprType
type EvalExprTreeM = Reader { varStore :: VarStore, buildPath :: Path -> Path }
type EvalExprTree = ExprTree ExprNode -> EvalExprTreeM (ExprTree ExprNode)

evalExprTree ::
  ExprTree ExprNode ->
  EvalExprTreeM (ExprTree ExprNode)
evalExprTree =
  cataM'
    (\x -> case x of
        Pure _ -> pure x
        Par _ -> pure x
        Seq _ -> pure x
        Keyed _ -> pure x
        Alt _ _ -> pure x
    )
    evalExprTreeF

evalExprTreeF ::
  ExprTreeF ExprNode (ExprTree ExprNode) ->
  EvalExprTreeM (ExprTree ExprNode)
evalExprTreeF = map embed <<< case _ of
  Pure node
    | NExpr status <- node
    , Unevaled expr <- status -> Pure <<< NExpr <$> do
        varStore <- asks _.varStore
        buildPath <- asks _.buildPath
        let path = buildPath PathNil
        case Data.Map.lookup path varStore of
          Nothing -> pure $ EvalFailed ""
          Just varMap -> case evalExpr varMap expr of
            Left err -> pure $ EvalFailed err
            Right exprType -> pure $ Evaled exprType
  x -> pure x

--------
-- UType
--------

data UType
  = UArray UType
  -- | USet UType
  -- | UMaybe UType
  -- | URecord (Object UType)
  -- | UVariant (Object UType)
  | UBoolean
  | UCents
  | UDateTime
  | UInt
  | UPair UType UType
  | UString
  | UUnit
  | UAny
derive instance genericUType :: Generic UType _
instance showUType :: Show UType where
  show x = genericShow x

newtype ExprType = ExprType (Mu ExprTypeF)
derive instance eqExprType :: Eq ExprType
derive instance genericExprType :: Generic ExprType _
derive instance newtypeExprType :: Newtype ExprType _
instance showExprType :: Show ExprType where
  show x = genericShow x

data ExprTypeF a
  = ArrayF (Array a)
  | BooleanF Boolean
  | IntF Int
  | PairF { name :: a, value :: a }
  | StringF String
derive instance eqExprTypeF :: Eq a => Eq (ExprTypeF a)
derive instance eq1ExprTypeF :: Eq1 ExprTypeF
derive instance functorExprTypeF :: Functor ExprTypeF
instance foldableExprTypeF :: Foldable ExprTypeF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    ArrayF xs -> foldMap f xs
    -- MaybeF ma -> foldMap f ma
    -- RecordF os -> foldMap f os
    BooleanF _ -> mempty
    IntF _ -> mempty
    PairF x -> f x.name <> f x.value
    StringF _ -> mempty
instance traversableExprTypeF :: Traversable ExprTypeF where
  sequence = traverse identity
  traverse f = case _ of
    ArrayF xs -> ArrayF <$> traverse f xs
    -- MaybeF ma -> MaybeF <$> traverse f ma
    -- RecordF os -> RecordF <$> traverse f os
    BooleanF x -> pure (BooleanF x)
    IntF x -> pure (IntF x)
    PairF x -> ado
      name <- f x.name
      value <- f x.value
      in PairF { name, value }
    StringF x -> pure (StringF x)
derive instance genericExprTypeF :: Generic (ExprTypeF a) _
instance showExprTypeF :: Show a => Show (ExprTypeF a) where
  show x = genericShow x

{- catamorphism-fusion law
h <<< f = g <<< map h
=>
h <<< cata f = cata g
where
  f :: f a -> a
  g :: f b -> b
  h :: a -> b

(h :: a -> b) <<< (f :: f a -> a) :: f a -> b
  =
(g :: f b -> b) <<< (map h :: f a -> f b) :: f a -> b

(h :: a -> b) <<< (cata f :: Fix f -> a) :: Fix f -> b
  =
cata g :: Fix f -> b
-}

{- catamorphism compose law (distributivity?)
cata f <<< cata (Fix <<< h) = cata (f <<< h)
where
  f :: f a -> a
  h :: g a -> f a
-}

{- banana-split theorem

cata f &&& cata g
  =
cata ((f <<< map fst) &&& (g <<< map snd))
-}

main :: Effect Unit
main = do
  log "🍝"
