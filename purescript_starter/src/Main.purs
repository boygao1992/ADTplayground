module Main where

import Effect

import Effect.Class.Console (log)
import Prelude (class Show, Unit, otherwise, pure, show, ($), (*), (+), (-), (==), (>))

import Data.Eq (class Eq)
import Data.Semiring (class Semiring)
import Data.Functor (class Functor)
import Control.Semigroupoid ((<<<))
import Control.Plus (class Plus, empty)
import Control.Applicative (class Applicative)
import Data.Generic.Rep as G
import Data.Generic.Rep.Show as GShow

import Control.Lazy (
  fix -- forall l. Lazy l => (l -> l) -> l
)

data Msg
  = Inc
  | Dec

update :: Msg -> Int -> Int
update Inc x = x + 1
update Dec x = x - 1

whoIsGreater :: Int -> Int -> Int
whoIsGreater n m
  | n > m = n
  | otherwise = m

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

newtype Person = Person
  { name :: String
  , age  :: Int
  }

derive instance genericPerson :: G.Generic Person _

instance showPerson :: Show Person where
  -- show (Person { name, age }) =
  --   "Person { name: " <> show name <> ", age: " <> show age <> "}"
  show person = GShow.genericShow person

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


factorial' :: Int -> Int
          -- fix :: ((Int -> Int) -> Int -> Int) -> Int -> Int
factorial' = fix(\f n ->
             case n of
               0 -> 1
               _ -> n * f(n -1)
           )

func :: forall a b. Eq a => Semiring a => Applicative b => Plus b => a -> a -> a -> b (Array a)
func n x y = if x + y == n
              then pure [x,y]
              else empty

newtype Event a = Event ((a -> Effect Unit) -> Effect (Effect Unit))

instance functorEvent :: Functor Event where
  map f (Event e) = Event \k -> e (k <<< f)

main :: Effect Unit
main = do
  log $ show $ Person { name : "1", age : 1 }
