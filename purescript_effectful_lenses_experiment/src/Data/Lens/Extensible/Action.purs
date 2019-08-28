module Data.Lens.Extensible.Action where

import Prelude

import Data.Leibniz (type (~), coerceSymm, liftLeibniz)
import Data.Lens.Internal.Forget (Forget(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import MonadVar.Classes as Var
import Node.Encoding (Encoding (Base64))
import Node.FS.Sync as File

type Optic i o s t a b = i a b -> o s t

type Lens s t a b = forall p. Strong p => Optic p p s t a b

class Readable i a | i -> a where
  readable :: forall o s t b.  Optic i o s t a b -> o s t

class Writeable i where
  writeable :: forall o s t a b. Optic i o s t a b -> b -> o s t
infixr 4 writeable as .~

class Mutateable i where
  mutateable :: forall o s t a b. Optic i o s t a b -> (a -> b) -> o s t
infixr 4 mutateable as %~

class MutateableF i f | i -> f where
  mutateableF :: forall o s t a b. Optic i o s t a b -> (a -> f b) -> o s t
infixr 4 mutateableF as ^~

instance readableForget :: Readable (Forget a) a where
  readable _Lens = _Lens $ Forget identity

instance writeableFunction :: Writeable (->) where
  writeable _Lens b = _Lens $ const b

instance mutateableFunction :: Mutateable (->) where
  mutateable _Lens f = _Lens f

newtype Action action a b = Action (action a b)
derive newtype instance profunctorAction :: Profunctor action => Profunctor (Action action)
derive newtype instance strongAction :: Strong action => Strong (Action action)

instance readableAction :: Readable (Action (ReadAction a)) a where
  readable _Lens = _Lens $ Action readAction
instance writeableAction :: Writeable (Action AlterAction) where
  writeable _Lens b = _Lens $ Action $ WriteAction b
instance mutateableAction :: Mutateable (Action AlterAction) where
  mutateable _Lens f = _Lens $ Action $ MutateAction f
instance mutateableFAction :: MutateableF (Action (MutateFAction f)) f where
  mutateableF _Lens f = _Lens $ Action $ wrap f

data ReadAction r a b
  = ReadAction (r ~ a)
readAction = ReadAction identity :: forall a b. ReadAction a a b

data AlterAction a b
  = WriteAction b -- NOTE const b :: a -> b
  | MutateAction (a -> b)

instance profunctorAlterAction :: Profunctor AlterAction where
  dimap _ r (WriteAction b) = WriteAction $ r b
  dimap l r (MutateAction f) = MutateAction $ r <<< f <<< l

instance strongAlterAction :: Strong AlterAction where
  first (WriteAction b) = MutateAction $ first $ const b
  first (MutateAction f) = MutateAction $ first f

  second (WriteAction b) = MutateAction $ second $ const b
  second (MutateAction f) = MutateAction $ second f

newtype MutateFAction f a b = MutateFAction (Star f a b) -- NOTE a -> f b
instance newtypeMUtateFAction :: Newtype (MutateFAction f a b) (a -> f b) where
  wrap = MutateFAction <<< Star
  unwrap (MutateFAction (Star k)) = k
derive newtype instance profunctorMutateFAction :: Functor f => Profunctor (MutateFAction f)
derive newtype instance strongMutateFAction :: Functor f => Strong (MutateFAction f)

class Variable action m v r | action -> r where
  _Var :: forall a. Optic (Action action) (->) (v a) (m r) a a

instance variableReadAction ::
  Var.MonadRead m v => Variable (ReadAction a) m v a where
  _Var (Action (ReadAction proof)) var =
    Var.read $ coerceSymm (liftLeibniz proof) var

instance variableAlterAction ::
  Var.MonadMutate_ m v => Variable AlterAction m v Unit where
  _Var (Action (WriteAction b)) var = Var.write var b
  _Var (Action (MutateAction f)) var = Var.mutate_ var f

instance variableMutateFAction ::
  Var.MonadMutateM_ f m v => Variable (MutateFAction f) m v Unit where
  _Var (Action act) var = Var.mutateM_ var (unwrap act)

newtype WeakAction action a b = WeakAction (action a b)
derive newtype instance profunctorWeakAction ::
  Profunctor action => Profunctor (WeakAction action)

instance readableWeakAction :: Readable (WeakAction (ReadAction a)) a where
  readable _Lens = _Lens $ WeakAction readAction

instance writeableWeakAction :: Writeable (WeakAction WeakWriteAction) where
  writeable _Lens b = _Lens $ WeakAction $ WeakWriteAction b

instance mutateableWeakAction :: Mutateable (WeakAction ModifyAction) where
  mutateable _Lens f = _Lens $ WeakAction $ ModifyAction f

instance mutateableFWeakAction:: MutateableF (WeakAction (MutateFAction f)) f where
  mutateableF _Lens k = _Lens $ WeakAction (wrap k)

newtype WeakWriteAction a b = WeakWriteAction b
instance profunctorWeakWriteAction :: Profunctor WeakWriteAction where
  dimap _ r (WeakWriteAction b) = WeakWriteAction $ r b

newtype ModifyAction a b = ModifyAction (a -> b)
derive newtype instance profunctorModifyAction :: Profunctor ModifyAction

class File action r | action -> r where
  _File :: Optic (WeakAction action) (->) String (Effect r) String String

instance fileReadAction :: File (ReadAction a) a where
  _File (WeakAction (ReadAction proof)) file =
    coerceSymm (liftLeibniz proof) (File.readTextFile Base64 file)

instance fileWeakWriteAction :: File WeakWriteAction Unit where
  _File (WeakAction (WeakWriteAction b)) file = File.writeTextFile Base64 file b

instance fileModifyAction :: File ModifyAction Unit where
  _File (WeakAction (ModifyAction f)) file =
    File.readTextFile Base64 file # map f >>= File.writeTextFile Base64 file

_1 :: forall a b c. Lens (a /\ c) (b /\ c) a b
_1 = first

_2 :: forall a b c. Lens (c /\ a) (c /\ b) a b
_2 = second

_Of :: forall s a b. (s -> a) -> Lens s b a b
_Of get = lcmap get

viewOn :: forall s t a b. s -> Optic (Forget a) (Forget a) s t a b -> a
viewOn s _Lens = unwrap (readable _Lens) s
infixr 4 viewOn as ^.

