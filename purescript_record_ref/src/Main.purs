module Main where

import Prelude

import Control.Monad.ST (run) as ST
import Control.Monad.ST.Unsafe (unsafeReadSTRef)
import Data.Nullable (Nullable, null, notNull)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref as Ref
import Record.ST as RST
import Record.ST.Nested as RST
import Type.Data.Symbol (SProxy(..))

{- tying the knot in JS
var refA = { value : { b : null } }
var refB = { value : { a : null } }
refA.value.b = refB.value
refB.value.a = refA.value
var a = refA.value // { b : { a : [Circular] } }
var b = refB.value // { a : { b : [Circular] } }
-}

{- annihilate Ref in place (JS plan)
var unRef = ref => ref.value
var x = function (postRef) {
  return function () {
    return
      { name: "wenbo"
      , post: unRef(postRef)
      }
  }
}
var y = { value : null }
var z = x(y)
z() // => { name: 'wenbo', post: null }

y.value = { a : 1 }
z() // => { name: 'wenbo', post: { a: 1 } }
-}

{-
/*
    ST ::
      { a :: Nullable A
      , b :: Nullable B
      , c :: Nullable C
      }
newtype A = A
  { name :: String
  , toB :: Unit -> B
  , toC :: Unit -> C
  }

newtype B = B
  { name :: String
  , toA :: Unit -> A
  , toC :: Unit -> C
  }

newtype C = C
  { name :: String
  , toA :: Unit -> A
  , toB :: Unit -> B
  }
*/
var ST = { a: null, b: null, c: null }

/*
    aConstructor
      :: forall h r.
       . STRecord h
          ( b :: Nullable B
          , c :: Nullable C
          | r
          )
      -> ST h A
    aConstructor st = do
      b <- RST.pathPeekSTRef (SProxy :: SProxy "b") st
      c <- RST.pathPeekSTRef (SProxy :: SProxy "c") st
      pure ( A
        { name : "A"
        , toB : \_ -> RST.unsafeReadSTRef b
        , toC : \_ -> RST.unsafeReadSTRef c
        }
      )
*/
var aConstructor = ST =>
  ({ name : "a"
   , toB : () => ST.b
   , toC : () => ST.c
   })
var bConstructor = ST =>
  ({ name : "b"
   , toA : () => ST.a
   , toC : () => ST.c
   })
var cConstructor = ST =>
  ({ name : "c"
   , toA : () => ST.a
   , toB : () => ST.b
   })
ST.a = aConstructor(ST)
ST.b = bConstructor(ST)
ST.c = cConstructor(ST)

const { a, b, c } = ST
-}

main :: Effect Unit
main = do
  -- logShow $ ST.run do
  --   a <- RST.thaw { a : { b : { c : "wenbo" } } }
  --   c <- RST.pathPeek (SProxy :: SProxy "a.b") a
  --   RST.pathModify (SProxy :: SProxy "a.b.c") (const "robot") a
  --   pure c

  -- logShow $ ST.run do
  --   -- | Entanglement
  --   a <- RST.thaw { a : { b : { c : "wenbo" } } }
  --   c <- RST.pathPeekSTRecord (SProxy :: SProxy "a.b") a
  --   -- modify common fields of a and c from either will affect both
  --   RST.pathModify (SProxy :: SProxy "c") (const "robot") c
  --   RST.pathPeek (SProxy :: SProxy "a.b") a

  logShow $ ST.run do
    x <- RST.thaw { a : { b : { c : "wenbo" } } }
    y <- RST.thaw { b : { c : "robot" } }
    cSTRef <- RST.pathPeekSTRef (SProxy :: SProxy "a.b") x
    RST.pathModify (SProxy :: SProxy "b") (const (unsafeReadSTRef cSTRef)) y
    RST.pathModify (SProxy :: SProxy "a.b.c") (const "webot") x
    RST.pathPeek (SProxy :: SProxy "b.c") y

