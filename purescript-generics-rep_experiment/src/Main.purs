module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (log, logShow)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON as JSON
import Test.Assert (assertEqual)
import Type.Proxy (Proxy(..))

-- | Utils
class UntaggedSumRep rep where
  untaggedSumRep :: Foreign -> Foreign.F rep

instance untaggedSumRepSum ::
  ( UntaggedSumRep a
  , UntaggedSumRep b
  ) => UntaggedSumRep (Sum a b) where
    untaggedSumRep f
        = Inl <$> untaggedSumRep f
      <|> Inr <$> untaggedSumRep f

instance untaggedSumRepConstructor ::
  UntaggedSumRep a => UntaggedSumRep (Constructor name a) where
    untaggedSumRep f = Constructor <$> untaggedSumRep f

instance untaggedSumRepArgument ::
  JSON.ReadForeign a => UntaggedSumRep (Argument a) where
    untaggedSumRep f = Argument <$> JSON.readImpl f

-- | Status, sum type
data Status
  = Case1 Int
  | Case2 Boolean
  | Case3 String
  | Case4 String

derive instance genericStatus :: Generic Status _
instance eqStatus :: Eq Status where
  eq = genericEq
instance showStatus :: Show Status where
  show = genericShow
instance decodeJsonStatus :: DecodeJson Status where
  decodeJson = genericDecodeJson
instance encodeJsonStatus :: EncodeJson Status where
  encodeJson = genericEncodeJson

instance readForeignStatus :: JSON.ReadForeign Status where
  readImpl f = to <$> untaggedSumRep f


-- | State, product type
newtype State = State
  { id :: Int
  , name :: String
  , status :: Status
  }

derive instance genericState :: Generic State _
instance showState :: Show State where
  show = genericShow

instance readForeignState :: JSON.ReadForeign State where
  readImpl f = to <$> untaggedSumRep f

instance decodeJsonState :: DecodeJson State where
  decodeJson = genericDecodeJson

instance encodeJsonState :: EncodeJson State where
  encodeJson = genericEncodeJson

-- | main
testJSON :: String
testJSON =  " { \"id\": 0 , \"name\": \"wenbo\" , \"status\": \"robot\"} "

testObject :: State
testObject = State
  { id : 0
  , name : "wenbo"
  , status : Case3 "robot"
  }

defaultEncodingOfTestObject :: String
defaultEncodingOfTestObject = """
{"values":[{"status":{"values":["robot"],"tag":"Case3"},"name":"wenbo","id":0}],"tag":"State"}
"""

-- | Record

newtype Record1 = Record1
  { space :: String
  , monkey :: Int
  }
derive instance genericRecord1 :: Generic Record1 _

gRecord1 :: Constructor "Record1"
  (Argument
     { space :: String
     , monkey :: Int
     }
  )
gRecord1 = from (Record1 { space : "", monkey : 0 })

-- | Recursive Type

data List a
  = Nil
  | Cons a (List a)

derive instance genericList :: Generic (List a) _

instance showList :: Show a => Show (List a) where
  show x = genericShow x

-- gList ::
--   Sum (Constructor "Nil" NoArguments)
--       (Constructor "Cons"
--         (Product
--           (Argument Int)
--           (Argument (List Int))
--         )
--       )
-- gList = from (Cons 1 $ Cons 2 $ Cons 3 $ Nil)

-- data List a = Nil | Cons { head :: a, tail :: List a }

-- cons :: forall a. a -> List a -> List a
-- cons head tail = Cons { head, tail }

-- derive instance genericList :: Generic (List a) _

-- instance showList :: Show a => Show (List a) where
--   show x = genericShow x

-- gList :: Sum (Constructor "Nil" NoArguments)
--   (Constructor "Cons"
--      (Argument
--         { head :: Int
--         , tail :: List Int
--         }
--      )
--   )
-- gList = from $ cons 1 $ cons 2 $ cons 3 $ Nil

data ListF a t
  = ConsF a t
  | NilF
derive instance genericListF :: Generic (ListF a t) _

gListF ::
  forall a.
  Sum
  (Constructor "ConsF"
    (Product
      (Argument Int)
      (Argument
        (ListF Int (ListF Int (ListF Int a))) -- this works
      )
    )
  )
  (Constructor "NilF" NoArguments)
gListF = from (ConsF 1 $ ConsF 2 $ ConsF 3 $ NilF :: ListF Int a)

instance showListF :: (Show a, Show t) => Show (ListF a t) where
  show = genericShow

gListF2 :: Sum (Constructor "ConsF" (Product (Argument Int) (Argument (ListF String (ListF Char (ListF Int Unit)))))) (Constructor "NilF" NoArguments)
gListF2 = from (ConsF 1 $ ConsF "2" $ ConsF '3' $ NilF :: ListF Int Unit)

-- | Derive Static Value
class StaticValue a where
  value :: a

instance staticValueInt :: StaticValue Int where
  value = 0
instance staticValueUnit :: StaticValue Unit where
  value = unit
instance staticValueString :: StaticValue String where
  value = "wenbo"

-- | Function

newtype Func = Func (Int -> String)

derive instance genericFunc :: Generic Func _

gFunc :: Constructor "Func" (Argument (Function Int String))
gFunc = from $ Func (show)

-- | Destructuring Function Type (used to be under `kind Arrow`)
--   | Function :: Type -> Type -> Type
--   | infixr 9 type Function as ->
--   |   right-associative
--   | type (->) :: Type -> Type -> Type
class FirstArgument f a | f -> a

instance firstArgumentFunc ::
  FirstArgument (a -> b) a

firstArgument :: forall f a. FirstArgument f a => Proxy f -> Proxy a
firstArgument _ = Proxy :: Proxy a

firstArgumentExample1 :: Proxy Int
firstArgumentExample1 = firstArgument (Proxy :: Proxy (Int -> String))

firstArgumentExample2 :: Proxy Int
firstArgumentExample2 = firstArgument (Proxy :: Proxy (Int -> String -> Char))

class SecondArgument f b | f -> b

instance secondArgumentFunc ::
  SecondArgument (a -> b) b

secondArgument :: forall f a. SecondArgument f a => Proxy f -> Proxy a
secondArgument _ = Proxy :: Proxy a

secondArgumentExample1 :: Proxy String
secondArgumentExample1 = secondArgument (Proxy :: Proxy (Int -> String))

secondArgumentExample2 :: Proxy (String -> Char)
secondArgumentExample2 = secondArgument (Proxy :: Proxy (Int -> String -> Char))

main :: Effect Unit
main = do
  -- case (JSON.readJSON testJSON) of
  --   Right ((State r) :: State) -> do
  --     assertEqual { expected: 0, actual: r.id }
  --     assertEqual { expected: "wenbo", actual: r.name }
  --     assertEqual { expected: Case3 "robot", actual: r.status } -- both Case3 and Case4 has the correct Argument type, but it returns the first match
  --     logShow r
  --   Left e -> do
  --     assertEqual { expected: "failed", actual: show e}

  -- case (jsonParser testJSON) of
  --   Left e ->
  --     log e
  --   Right json -> do
  --     logShow $ (decodeJson json) :: Either String State

  -- log $ stringify $ encodeJson testObject

  logShow $ Cons 1 $ Cons 2 $ Cons 3 $ Nil
  -- TODO infinite loop (?)
  -- RangeError: Maximum call stack size exceeded
  --   at new GenericShowArgs (/purescript-generics-rep_experiment/.psci_modules/node_modules/Data.Generic.Rep.Show/index.js:12:32)
  --   at Object.genericShowArgsArgument (/purescript-generics-rep_experiment/.psci_modules/node_modules/Data.Generic.Rep.Show/index.js:19:12)
  --   at showList (/purescript-generics-rep_experiment/.psci_modules/node_modules/Main/index.js:260:227)
  --   at showList (/purescript-generics-rep_experiment/.psci_modules/node_modules/Main/index.js:260:308)

  -- logShow $ cons 1 $ cons 2 $ cons 3 $ Nil

  logShow $ ConsF 1 $ ConsF 2 $ ConsF 3 $ NilF :: ListF Int Unit
  -- (ConsF 1 (ConsF 2 (ConsF 3 NilF)))
