module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.Function.Uncurried (Fn1, runFn1)

newtype User = User
  { name :: String
  , posts :: Ref (Array Post)
  }

newtype Post = Post
  { id :: String
  , author :: Ref (Maybe User)
  }
instance showPost :: Show Post where
  show (Post p) = "Post { " <> show p.id <> " }"

-- | without class Lazy
newtype A = A
  { b :: B }
newtype B = B
  { a :: A }

type Lazy a = Unit -> a

step :: forall a. Lazy a -> a
step l = l unit

a0 :: Lazy A
a0 = \_ -> A
  { b : step b0 }

b0 :: Lazy B
b0 = \_ -> B
  { a : step a0 }


foreign import _unRef :: forall a. Fn1 (Ref a) a
-- NOTE Ref.modify doesn't mutate the referred object in place but creates a new one and points the contained reference to the new object, so this won't work as expected.
-- TODO have to unwrap the Ref object in JS
-- or build my own Ref implementation
unRef :: forall a. Ref a -> a
unRef = runFn1 _unRef

{- TODO

derive a GenericGraphQLType constructor for each user-defined type
- which takes a Record of GenericGraphQLType as dependency

e.g.
type User_Paritioned =
  { scalars :: { ... }
  , relations :: { posts :: Array Post }
  }

userTypeConstructor :: Ref (ObjectType ctx (Array Post)) -> ObjectType ctx (Maybe User)

type Post_Partitioned =
  { scalars :: { ... }
  , relations :: { author :: User }
  }

postTypeConstructor :: Ref (ObjectType ctx User) -> ObjectType ctx (Maybe Post)

-}

-- | TODO
data GraphQLObjectType ctx a

class OneToOne a b where
  oneToOne :: { first :: (forall ctx. GraphQLObjectType ctx a)
              , second :: (forall ctx. GraphQLObjectType ctx b)
              }

class OneToMany a b where
  oneToMany :: { first :: (forall ctx. GraphQLObjectType ctx a)
               , second :: (forall ctx. GraphQLObjectType ctx b)
               }

class ManyToMany a b where
  manyToMany :: { first :: (forall ctx. GraphQLObjectType ctx a)
                , second :: (forall ctx. GraphQLObjectType ctx b)
                }


main :: Effect Unit
main = do
  -- posts <- Ref.new []
  -- author <- Ref.new Nothing

  -- let wenbo = User { name : "wenbo", posts }
  --     post1 = Post { id : "001", author }
  --     wenbo2 = case wenbo of
  --       User r -> User r { name = "wenbo2"}
  -- Ref.modify_ (const $ Just wenbo) author
  -- Ref.modify_ (const $ [ post1 ]) posts

  -- logShow $ case wenbo2 of
  --   User r -> unRef r.posts

  objectRef <- Ref.new { a : { b : "wenbo" }, c : "robot" }
  let x = _.a $ unRef objectRef
  Ref.modify_ (_ { a { b = "wenbo1" } }) objectRef
  logShow x -- { b : "wenbo" }

  pure unit

