module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, Product, Sum(..), from, to)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (logShow)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Row as Row
import Type.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Record.Builder (Builder)
import Record.Builder as Builder

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

-- | Universal Type to one static Type
class TypeDescription a where
  description :: Proxy a -> String

instance typeDescriptionInt :: TypeDescription Int where
  description _ = "Int"

instance typeDescriptionString :: TypeDescription String where
  description _ = "String"

-- | Universal Type to type class inferred Type
class MapType a b | a -> b

instance mapTypeInt2String :: MapType Int String
instance mapTypeString2Int :: MapType String Int

class MapTypeWithProxy a b | a -> b where
  mapTypeWithProxy :: Proxy a -> Proxy b

instance mapTypeWithProxyInt :: MapTypeWithProxy Int String where
  mapTypeWithProxy _ = Proxy :: Proxy String

class WithMapType a where
  withMapType :: forall b. MapType a b => b

-- instance withMapTypeInt :: WithMapType Int where
--   withMapType = "" -- TypeError: could not match type String with type b0
-- NOTE Compiler doesn't utilize MapType to uniquely infer type b when defining a type class instance for WithMapType.
-- I guess it's reasonable since compiler doesn't know which type class inference to prioritize without hierarchical relations specified

class MapType a b <= WithMapType2 a b | a -> b where
  withMapType' :: MapType a b => b

-- NOTE Compiler will guard against instances like WithMapType2 Int Int because of the type class inheritance from MayType.
-- but we need to explicitly state the inferred type which makes this unusable
instance withMapType2Int :: WithMapType2 Int String where
  withMapType' = ""

-- | Encode Description as Type-level Literal

class IsField (row :: # Type) (name :: Symbol)

class IsField row name <= InjectDescription (row :: # Type) (name :: Symbol) (des :: Symbol) | row name -> des -- multiple distinct descriptions for the same field under the same row are not allowed

-- e.g.
-- InjectDescription (id :: String) "id" "This is a unique identifier for ..."

-- | NoArg Type Class
-- NOTE able to carry static values
class TypeClassNoArg where
  name :: String

instance typeClassNoArg :: TypeClassNoArg where
  name = "wenbo"

-- | Dispatch Row fields in Arguments
-- | e.g.

{- gather first and second arguments of functions in a Row into two separate Row

rules for resolvers
- if a field has args definition in its fieldSpec
  - then its resolver is Required
  - else
    - if the field is Scalar
      , or the field is Relational and all its fields have a resolver Required
        - NOTE cyclic dependency alert
        - e.g. User { post :: Post }, Post { author :: User }
          to see if resolver for field `post` in User is Required
          we need to know if resolver for field `author` in Post is Required
          which then come back to the question itself
      - then its resolver is Optional
      - else its resolver is Required

updated rules for resolvers with cyclic dependency avoided
- if a field has args definition in its fieldSpec
  - then its resolver is Required
  - else
    - case1: the field is Scalar
      - then its resolver is Optional
    - case2: the field is Relational
      - then its resolver is Required
    - otherwise: TypeError

NOTE extra rules to forbid trivial resolvers to be Required
- an Entity must have at least one Scalar field
  - which doesn't have a resolver itself
  - but is resolved from its parent
e.g. User { id :: String, post :: Post }, Post { id :: { id :: String } -> String }
  the resolver for field `post` in User is trivial:
    postResolver :: { source:: {} } -> Aff {}
  which takes in an empty Record and produces an empty Record

newtype User = User
  { id :: String
  , posts :: { date :: String } -> Array Post
  , comments :: { limit :: Int } -> Array Comment
  }
newtype Post = Post
  { id :: String
  , author :: User
  , comments :: { limit :: Int } -> Array Comment
  }
newtype Comment = Comment
  { id :: String
  , author :: User
  , post :: Post
  }

variables
- Generic entity (Constructor entityName (Argument spec))
- spec :: # Type
  - source
  - argsRow
  - outputRow
  - fieldTypeRow
  - resolversSchema <- source, argsRow, outputRow
  - deps <- fieldTypeRow
- resolvers :: # Type
- deps :: # Type
- o = GraphQLType spec

source = { id :: String }

argsRow =
  { id :: NoArg
  , posts :: WithArgs { date :: String }
  , comments :: WithArgs { limit :: Int }
  }
outputRow =
  { id :: String
  , posts :: Array { id :: String }
  , comments :: Array { id :: String }
  }
fieldTypeRow =
  { id :: ScalarField
  , posts :: RelationalField Post
  , comments :: RelationalField Comment
  }
argsRow + outputRow =
  { id :: ObjectField NoArg String
  , posts :: ObjectField (WithArgs { date :: String }) (Array { id :: String })
  , comments :: ObjectField (WithArgs { id :: String }) (Array { id :: String })
  }

resolversSchema =
  { id :: Optional
      { source :: source
      }
      -> Aff String
  , posts :: Required
      { source :: source
      , args :: { date :: String }
      }
      -> Aff (Array postScalars)
  , comments :: Required
      { source :: source
      , args :: { limit :: Int }
      }
      -> Aff (Array commentScalars)
  }

deps =
  { "Post" :: Unit -> Nullable(GraphQLType Post)
  , "Comment" :: Unit -> Nullable(GraphQLType Comment)
  }

GraphQLType User =
G.object
{ name: "User"
, fields: \_ ->
  { id:
    { type: G.string }
  , posts:
    { type: G.list( deps."Post" unit )
    , args:
      { date: { type: G.string }
      }
    , resolve: resolvers.posts
    }
  , comments:
    { type: G.list( deps."Comment" unit )
    , args:
      { limit: { type: G.int }
      }
    , resolve: resolvers.comments
    }
  }
}



{ one :: { a :: Int } -> { x :: String } -> Unit
, two :: { a :: Int } -> { y :: String } -> Number
, three :: { b :: Int } -> { x :: String } -> Int
, four :: { b :: Int } -> { y :: String } -> String
}

=>

   { a :: Int, b :: Int }
-> { x :: String, y :: String }
-> { one :: Unit
   , two :: Number
   , three :: Int
   , four :: String
   }

-}

{- a simpler case

{ one :: { a :: Int } -> Unit
, two :: { a :: Int } -> Number
, three :: { b :: Int } -> Int
, four :: { b :: Int } -> String
}

=>

   { a :: Int, b :: Int }
-> { one :: Unit
   , two :: Number
   , three :: Int
   , four :: String
   }

-}

class MergeFunc (spec :: # Type) (i :: # Type) (o :: # Type) | spec -> i o

instance mergeFuncImpl ::
  ( RowList.RowToList spec rl
  , CollectArgs rl i' o
  , Row.Nub i' i
  ) => MergeFunc spec i o

class CollectArgs (rl :: RowList) (i :: # Type) (o :: # Type)| rl -> i o

instance collectArgsNil ::
  CollectArgs RowList.Nil () ()
else instance collectArgsCons ::
  ( CollectArgs restRl restI restO
  , Row.Union arg restI i
  , Row.Cons name output restO o
  ) => CollectArgs (RowList.Cons name ((Record arg) -> output) restRl) i o

-- class ApplyArgs (rl :: RowList) (spec :: # Type) (i :: # Type) (from :: # Type) (to :: # Type) | rl spec i -> from to
--   where
--     applyArgs :: RLProxy rl -> Record spec -> Record i -> Builder {|from} {|to}

-- instance applyArgsNil ::
--   ApplyArgs RowList.Nil spec i () ()
--   where
--     applyArgs _ _ _ = identity
-- else instance applyArgsCons ::
--   ( ApplyArgs restRl i from restTo
--   , Row.Cons name output restTo to
--   , Row.Lacks name restTo
--   , IsSymbol name
--   ) => ApplyArgs (RowList.Cons name ((Record arg) -> output) restRl) spec i from to
--   where
--     applyArgs _ spec i = 



-- Test
collectRowArgs :: forall spec i o. MergeFunc spec i o => RProxy spec -> Proxy (Record i -> Record o)
collectRowArgs _ = Proxy :: Proxy (Record i -> Record o)

collectExample1 :: Proxy
  ({ a :: Int
   , b :: Int
   }
   -> { four :: String
      , one :: Unit
      , three :: Int
      , two :: Number
      }
  )
collectExample1 = collectRowArgs
                    (RProxy :: RProxy
                               ( one :: { a :: Int } -> Unit
                               , two :: { a :: Int } -> Number
                               , three :: { b :: Int } -> Int
                               , four :: { b :: Int } -> String
                               )
                    )




main :: TypeClassNoArg => Effect Unit
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

  logShow $ name
