module Main where

import Prelude

import Data.Nullable (Nullable, null, notNull)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref as Ref
import RecordRef as RecordRef
import Type.Data.Symbol (SProxy(..))

{- tying the knot in JS
var refA = { value : { b : null } }
var refB = { value : { a : null } }
refA.value.b = refB.value
refB.value.a = refA.value
var a = refA.value // { b : { a : [Circular] } }
var b = refB.value // { a : { b : [Circular] } }
-}

main :: Effect Unit
main = do
  -- rec <- RecordRef.new { a: { b: "wenbo" } }
  -- RecordRef.pathWrite
  --   (SProxy :: SProxy "")
  --   {a : { b : "webot" } }
  --   rec
  -- RecordRef.pathModify_
  --   (SProxy :: SProxy "a")
  --   (const $ { b : "wenbo" })
  --   rec
  -- RecordRef.pathModify_
  --   (SProxy :: SProxy "a.b")
  --   (const "robot")
  --   rec
  -- b <- RecordRef.pathRead (SProxy :: SProxy "a") rec
  -- logShow b

  -- | Entanglement
  x <- RecordRef.new { x : { y : "wenbo" } }
  y <- RecordRef.pathReadRef (SProxy :: SProxy "x") x
  -- modify common fields of x and y from either will affect both
  RecordRef.pathModify_ (SProxy :: SProxy "y") (const "robot") y
  robot <- RecordRef.read x
  logShow robot
  RecordRef.pathModify_ (SProxy :: SProxy "x.y") (const "wenbo") x
  wenbo <- RecordRef.read y
  logShow wenbo


  -- x <- RecordRef.new { y : null }
  -- y <- RecordRef.new { x : null }
  -- RecordRef.pathModify_
  --   (SProxy :: SProxy "y")
  --   (const $ notNull y)
  --   x

  -- currently only support Record which will blow the type inference if circular
  -- TODO introduce newtype wrapping for circular record

{-
 No type class instance was found for

  Prim.Row.Cons "y"
                (Nullable
                  (RecordRef
                      ( x :: forall a. Nullable a
                      )
                  )
                )
                t4
                ( y :: forall a. Nullable a
                )

The instance head contains unknown type variables. Consider adding a type annotation.

while applying a function pathModify_
of type ParsePath t0 t1 => RowPListAccess t2 t1 t3 => PListToArray t1 => SProxy t0 -> (t3 -> t3) -> RecordRef t2 -> Effect Unit
to argument SProxy
while inferring the type of pathModify_ SProxy
in value declaration main

where t3 is an unknown type
    t1 is an unknown type
    t0 is an unknown type
    t2 is an unknown type
    t4 is an unknown type
 (psc-ide)

-}

