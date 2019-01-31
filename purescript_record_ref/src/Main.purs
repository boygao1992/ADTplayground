module Main where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST (run) as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Nullable (Nullable, null, notNull)
import Effect (Effect)
import Effect.Console (logShow)
import Record as Record
import Record.Builder as Builder
import Record.ST as RST
import Record.ST.Nested as RST
import Type.Data.Symbol (SProxy(..))
import Record.Builder (Builder)

{- tying the knot in JS with direct mutations
var refA = { value : { b : null } }
var refB = { value : { a : null } }
refA.value.b = refB.value
refB.value.a = refA.value
var a = refA.value // { b : { a : [Circular] } }
var b = refB.value // { a : { b : [Circular] } }
-}

{- tying the know in JS with indirect mutations
var ST = { a: null, b: null, c: null }

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

newtype A = A
  { name :: String
  , toB :: Unit -> Nullable B
  , toC :: Unit -> Nullable C
  }

aConstructor
  :: forall h
   . { "B" :: Unit -> Nullable B, "C" :: Unit -> Nullable C }
  -> ST h A
aConstructor ref =
  pure (
    A { name : "A"
      , toB : Record.get (SProxy :: SProxy "B") ref
      , toC : Record.get (SProxy :: SProxy "C") ref
      }
  )

newtype B = B
  { name :: String
  , toA :: Unit -> Nullable A
  , toC :: Unit -> Nullable C
  }

bConstructor
  :: forall h
   . { "A" :: Unit -> Nullable A, "C" :: Unit -> Nullable C }
  -> ST h B
bConstructor ref =
  pure (
    B { name : "B"
      , toA : Record.get (SProxy :: SProxy "A") ref
      , toC : Record.get (SProxy :: SProxy "C") ref
      }
  )

newtype C = C
  { name :: String
  , toA :: Unit -> Nullable A
  , toB :: Unit -> Nullable B
  }

cConstructor
  :: forall h
   . { "A" :: Unit -> Nullable A, "B" :: Unit -> Nullable B }
  -> ST h C
cConstructor ref =
  pure (
    C { name : "C"
      , toA : Record.get (SProxy :: SProxy "A") ref
      , toB : Record.get (SProxy :: SProxy "B") ref
      }
  )

-- | OpenRecord, r = open row
class OpenRecord (r :: # Type) typ (o :: # Type) | r typ -> o

instance openRecordImpl ::
  OpenRecord r typ  (self :: typ | r)

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

  -- logShow $ ST.run do
  --   x <- RST.thaw { a : { b : { c : "wenbo" } } }
  --   y <- RST.thaw { b : { c : "robot" } }
  --   cLazyRef <- RST.pathPeekLazyRef (SProxy :: SProxy "a.b") x
  --   RST.pathModify (SProxy :: SProxy "b") (\_ -> cLazyRef unit) y
  --   -- now (x.a.b) and (y.b) are pointing to the same object { c : "wenbo" }
  --   RST.pathModify (SProxy :: SProxy "a.b.c") (const "webot") x
  --   -- x.a.b.c are mutated in place and this mutation propagates to y.b.c through shared reference
  --   -- now y.b.c = "webot" as well
  --   RST.pathPeek (SProxy :: SProxy "b.c") y

  let output
        = ST.run do
            let init = Builder.build
                        (   Builder.insert (SProxy :: SProxy "A") (null :: Nullable A)
                        >>> Builder.insert (SProxy :: SProxy "B") (null :: Nullable B)
                        >>> Builder.insert (SProxy :: SProxy "C") (null :: Nullable C)
                        )
                        {}

            deps <- RST.thaw init

            let stBuilder = identity :: Builder {} {}
            aRef <- RST.peekLazyRef (SProxy :: SProxy "A") deps
            let stBuilder0 = Builder.insert (SProxy :: SProxy "A") aRef <<< stBuilder
            bRef <- RST.peekLazyRef (SProxy :: SProxy "B") deps
            let stBuilder1 = Builder.insert (SProxy :: SProxy "B") bRef <<< stBuilder0
            cRef <- RST.peekLazyRef (SProxy :: SProxy "C") deps
            let stBuilder2 = Builder.insert (SProxy :: SProxy "C") cRef <<< stBuilder1
            let st = Builder.build
                        stBuilder2
                        {}

            let aSt = Builder.build
                        (   Builder.insert
                              (SProxy :: SProxy "B")
                              (Record.get (SProxy :: SProxy "B") st)
                        >>> Builder.insert
                              (SProxy :: SProxy "C")
                              (Record.get (SProxy :: SProxy "C") st)
                        )
                        {}
            a <- aConstructor aSt
            RST.modify (SProxy :: SProxy "A") (const (notNull a)) deps

            let bSt = Builder.build
                        (   Builder.insert
                              (SProxy :: SProxy "A")
                              (Record.get (SProxy :: SProxy "A") st)
                        >>> Builder.insert
                              (SProxy :: SProxy "C")
                              (Record.get (SProxy :: SProxy "C") st)
                        )
                        {}
            b <- bConstructor bSt
            RST.modify (SProxy :: SProxy "B") (const (notNull b)) deps

            let cSt = Builder.build
                        (   Builder.insert
                              (SProxy :: SProxy "A")
                              (Record.get (SProxy :: SProxy "A") st)
                        >>> Builder.insert
                              (SProxy :: SProxy "B")
                              (Record.get (SProxy :: SProxy "B") st)
                        )
                        {}
            c <- cConstructor cSt
            RST.modify (SProxy :: SProxy "C") (const (notNull c)) deps

            pure st

  pure unit


