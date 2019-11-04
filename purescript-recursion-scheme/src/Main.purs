module Main where


import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Foldable as Data.Foldable
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Foreign.Object
import Matryoshka (class Corecursive, class Recursive, embed, project)
import Matryoshka as Matryoshka

------------
-- Recursive
------------

-- class Functor f <= Recursive t f | t → f where
--   project :: t → f t

-- instance recursiveMu :: Functor f => Recursive (Mu f) f where
--   project = un In

instance recursiveList :: Recursive (List x) (ListF x) where
  project :: List x -> ListF x (List x)
  project x =    -- ListF x (List x)
    map List   -- ListF x (Mu (ListF x))
    <<< project -- Mu (ListF x)
    <<< un List -- List x
    $ x

instance recursiveUType :: Recursive UType UTypeF where
  project x = map UType <<< project <<< un UType $ x

instance recursiveExprType :: Recursive ExprType ExprTypeF where
  project x = map ExprType <<< project <<< un ExprType $ x

{-

instance recursiveExprTree :: Recursive (ExprTree x) (ExprTreeF x) where
  project x = map ExprTree <<< project <<< un ExprTree $ x

-}

--------------
-- Corecursive
--------------

-- class Functor f <= Corecursive t f | t → f where
--   embed :: f t → t

-- instance corecursiveMu :: Functor f => Corecursive (Mu f) f where
--   embed = In

instance corecursiveList :: Corecursive (List x) (ListF x) where
  embed :: ListF x (List x) -> List x
  embed x =            -- List x
    List             -- Mu (ListF x)
    <<< embed         -- ListF x (Mu (ListF x))
    <<< map (un List) -- ListF x (List x)
    $ x

instance corecursiveUType :: Corecursive UType UTypeF where
  embed x = UType <<< embed <<< map (un UType) $ x

instance corecursiveExprType :: Corecursive ExprType ExprTypeF where
  embed x = ExprType <<< embed <<< map (un ExprType) $ x

{-

instance corecursiveExprTree :: Corecursive (ExprTree x) (ExprTreeF x) where
  embed x = ExprTree <<< embed <<< map (un ExprTree) $ x

-}

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

{-
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
-}

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
  Recursive t f => -- t = Fix f ~ f (f (f ... (f a) ... ))
  Recursive u g => -- u = Fix g ~ g (g (g ... (g b) ... ))
  (f (u -> c) -> g u -> c) -> -- f (Fix g -> c) -> g (Fix g) -> c
  t -> u -> c -- Fix f -> Fix g -> c
zipo zAlg =
  (Matryoshka.cata :: (f (u -> c) -> (u -> c)) -> t -> (u -> c)) -- Fix f -> (Fix g -> c)
    -- f (Fix g -> c) -> (Fix g -> c)
    \(x :: f (u -> c)) -- f (Fix g -> c)
    -> -- c
      zAlg x -- g (Fix g)
      <<< project -- Fix g

-------
-- List
-------

newtype List x = List (Mu (ListF x))
data ListF x a
  = Nil
  | Cons x a
derive instance newtypeList :: Newtype (List x) _
derive instance functorListF :: Functor (ListF x)

nil_ :: forall x. List x
nil_ = embed Nil

cons_ :: forall x. x -> List x -> List x
cons_ x = embed <<< Cons x

sum :: List Int -> Int
sum = Matryoshka.cata sumF
  where
  sumF :: ListF Int Int -> Int
  sumF = case _ of
    Nil -> 0
    Cons x acc -> x + acc

{-

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

-}

--------
-- UType
--------

utypeCheck :: UType -> ExprType -> Boolean
utypeCheck = zipo utypeCheckF

utypeCheckF :: UTypeF (ExprType -> Boolean) -> ExprTypeF ExprType -> Boolean
utypeCheckF = case _, _ of
  UArray p, ArrayF xs -> Data.Foldable.all p xs
  UBoolean, BooleanF _ -> true
  UInt, IntF _ -> true
  URecord ps, RecordF xs ->
    foldlWithIndex
      ( \key acc x -> case Foreign.Object.lookup key ps of
          Nothing -> false
          Just p -> acc && p x
      )
      true
      xs
  UString, StringF _ -> true
  UVariant ps, VariantF key x -> case Foreign.Object.lookup key ps of
    Nothing -> false
    Just p -> p x
  UAny, _ -> true
  _, _ -> false
  where
  uTypeTotalityProof :: forall a. UTypeF a -> UTypeF a
  uTypeTotalityProof x = case x of
    UArray _ -> x
    UBoolean -> x
    UInt -> x
    URecord _ -> x
    UString -> x
    UVariant _ -> x
    UAny -> x
  exprTypeTotalityProof :: forall a. ExprTypeF a -> ExprTypeF a
  exprTypeTotalityProof x = case x of
    ArrayF _ -> x
    BooleanF _ -> x
    IntF _ -> x
    RecordF _ -> x
    StringF _ -> x
    VariantF _ _ -> x

newtype UType = UType (Mu UTypeF)
derive instance newtypeUType :: Newtype UType _
derive instance genericUType :: Generic UType _
instance showUType :: Show UType where
  show x = genericShow x
derive instance eqUType :: Eq UType

uArray :: UType -> UType
uArray = embed <<< UArray

uBoolean :: UType
uBoolean = embed UBoolean

uInt :: UType
uInt = embed UInt

uRecord :: Object UType -> UType
uRecord = embed <<< URecord

uString :: UType
uString = embed UString

uAny :: UType
uAny = embed UAny

data UTypeF a
  = UArray a
  | UBoolean
  | UInt
  | URecord (Object a)
  | UString
  | UVariant (Object a)
  | UAny
derive instance genericUTypeF :: Generic (UTypeF a) _
instance showUTypeF :: Show a => Show (UTypeF a) where
  show x = genericShow x
derive instance eqUTypeF :: Eq a => Eq (UTypeF a)
derive instance eq1UTypeF :: Eq1 UTypeF
derive instance functorUTypeF :: Functor UTypeF
instance foldableUTypeF :: Foldable UTypeF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    UArray xs -> f xs
    UBoolean -> mempty
    UInt -> mempty
    URecord os -> foldMap f os
    UString -> mempty
    UVariant xs -> foldMap f xs
    UAny -> mempty
instance traversableUTypeF :: Traversable UTypeF where
  sequence = traverse identity
  traverse f = case _ of
    UArray xs -> UArray <$> f xs
    UBoolean -> pure UBoolean
    UInt -> pure UInt
    URecord os -> URecord <$> traverse f os
    UString -> pure UString
    UVariant xs -> UVariant <$> traverse f xs
    UAny -> pure UAny

-----------
-- ExprType
-----------

newtype ExprType = ExprType (Mu ExprTypeF)
derive instance eqExprType :: Eq ExprType
derive instance genericExprType :: Generic ExprType _
derive instance newtypeExprType :: Newtype ExprType _
instance showExprType :: Show ExprType where
  show x = genericShow x

array_ :: Array ExprType -> ExprType
array_ = embed <<< ArrayF

boolean_ :: Boolean -> ExprType
boolean_ = embed <<< BooleanF

int_ :: Int -> ExprType
int_ = embed <<< IntF

record_ :: Object ExprType -> ExprType
record_ = embed <<< RecordF

string_ :: String -> ExprType
string_ = embed <<< StringF

inj_ :: String -> ExprType -> ExprType
inj_ key = embed <<< VariantF key

data ExprTypeF a
  = ArrayF (Array a)
  | BooleanF Boolean
  | IntF Int
  | RecordF (Object a)
  | StringF String
  | VariantF String a
derive instance eqExprTypeF :: Eq a => Eq (ExprTypeF a)
derive instance eq1ExprTypeF :: Eq1 ExprTypeF
derive instance functorExprTypeF :: Functor ExprTypeF
instance foldableExprTypeF :: Foldable ExprTypeF where
  foldl f z x = foldlDefault f z x
  foldr f z x = foldrDefault f z x
  foldMap f = case _ of
    ArrayF xs -> foldMap f xs
    BooleanF _ -> mempty
    IntF _ -> mempty
    RecordF os -> foldMap f os
    StringF _ -> mempty
    VariantF key x -> f x
instance traversableExprTypeF :: Traversable ExprTypeF where
  sequence = traverse identity
  traverse f = case _ of
    ArrayF xs -> ArrayF <$> traverse f xs
    BooleanF x -> pure (BooleanF x)
    IntF x -> pure (IntF x)
    RecordF os -> RecordF <$> traverse f os
    StringF x -> pure (StringF x)
    VariantF key x -> VariantF key <$> f x
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
