module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Lazy (Lazy, defer)
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

foreign import _processRef :: forall a. Fn1 (Ref a) a

-- HACK
processRef :: forall a. Ref a -> a
processRef = runFn1 _processRef

newtype A = A
  { b :: Lazy B }
newtype B = B
  { a :: Lazy A }

a0 :: Lazy A
a0 = defer \_ -> A { b : b0 }

b0 :: Lazy B
b0 = defer \_ -> B { a : a0 }

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
  posts <- Ref.new []
  author <- Ref.new Nothing

  let wenbo = User { name : "wenbo", posts }
      post1 = Post { id : "001", author }
  Ref.modify_ (const $ Just wenbo) author
  Ref.modify_ (const $ [ post1 ]) posts

  logShow $ processRef posts
  pure unit

