module Control.Applicative.Free where

import Prelude

import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Rec.Class (Step(..), tailRec)

data FreeAp f a
  = Pure a -- pure :: a -> g a
  | Lift (f a) -- nat :: f a -> g a
  | Ap (FreeAp f (Val -> a)) (FreeAp f Val) -- stacking Apply action in this Free Algebra

data Val

type Stack f g =
  { funcStack :: FuncStack g
  , valStack :: ValStack f
  }
type FuncStack g = List (ApFunc g)
type ApFunc g =
  { func :: g (Val -> Val)
  , count :: Int
  }
type ValStack f = NEL.NonEmptyList (FreeAp f Val)

mkAp
  :: forall f i a
  . FreeAp f (i -> a)
  -> FreeAp f i
  -> FreeAp f a
mkAp fia fi = Ap (coerceFunc fia) (coerceVal fi)
  where
    -- NOTE erase type info after this type validation, to simplify type signatures
    -- able to do this, because these functions/values are restricted to applicative computation locally
    -- there is no difference between
    --   apply (FreeAp f (i -> a)) (FreeAp f i) = FreeAp a
    -- and
    --   apply (FreeAp f (Val -> a)) (FreeAp f Val) = FreeAp a

    coerceFunc :: FreeAp f (i -> a) -> FreeAp f (Val -> a)
    coerceFunc = unsafeCoerce

    coerceVal :: FreeAp f i -> FreeAp f Val
    coerceVal = unsafeCoerce

goLeft
  :: forall f g
  . Applicative g
  => Stack f g
  -> (f ~> g)
  -> FreeAp f (Val -> Val)
  -> Int
  -> Stack f g
goLeft { funcStack, valStack } nat freeap count = case freeap of
  Pure a ->
    { funcStack: ( { func: pure a, count } `Cons` funcStack )
    , valStack
    }
  Lift a ->
    { funcStack: ( { func: nat a, count } `Cons` funcStack )
    , valStack
    }
  Ap l r ->
    goLeft
      { funcStack
      , valStack: r `NEL.cons` valStack
      }
      nat
      (unsafeToFunc l)
      (count + 1)
    where
      unsafeToFunc :: FreeAp f (Val -> (Val -> Val)) -> FreeAp f (Val -> Val)
      unsafeToFunc = unsafeCoerce

goApply
  :: forall f g
  . Applicative g
  => FuncStack g
  -> List (FreeAp f Val)
  -> g Val
  -> Step (Stack f g) (g Val)
goApply funcStack vals gVal = case funcStack of
  Nil -> Done gVal
  Cons f fs ->
    let gRes = f.func <*> gVal
    in
      if f.count == 1
      then case fs of
        Nil -> Done gRes
        _ -> goApply fs vals gRes
      else case vals of
        Nil -> Done gRes
        Cons val vals' ->
          Loop
          { funcStack: ( { func: unsafeToGFunc gRes, count: f.count - 1 }
                         `Cons` fs )
          , valStack: NEL.NonEmptyList (val :| vals')
          }
  where
    unsafeToGFunc :: g Val -> g (Val -> Val)
    unsafeToGFunc = unsafeCoerce

foldFreeAp
  :: forall f g a
  . Applicative g
  => (f ~> g)
  -> FreeAp f a -> g a
foldFreeAp nat freeap =
  unsafeToG
  $ tailRec go
      { funcStack: Nil
      , valStack: NEL.singleton $ unsafeToFVal freeap
      }
  where
    unsafeToFVal :: forall f' a'. FreeAp f' a' -> FreeAp f' Val
    unsafeToFVal = unsafeCoerce

    unsafeToG :: g Val -> g a
    unsafeToG = unsafeCoerce

    go :: Stack f g -> Step (Stack f g) (g Val)
    go { funcStack, valStack: NEL.NonEmptyList (val :| vals) } =
      case val of
        Pure a -> goApply funcStack vals (pure a)
        Lift f -> goApply funcStack vals (nat f)
        Ap l r ->
          let
            stack =
              { funcStack
              , valStack: NEL.NonEmptyList (r :| vals)
              }
          in
            Loop $ goLeft stack nat l 1
