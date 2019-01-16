module Main where

import Prelude

import Data.Maybe (Maybe)
import Data.Array (head) as A
import Data.Lazy (Lazy, defer, force)
import Data.List.Lazy (List(..), Step(..))
import Data.List.Lazy (take) as L
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Console (logShow)

incrementalSequence :: Int -> List Int
incrementalSequence x = List $ defer \_ -> Cons x $ incrementalSequence (x + 1)

newtype User = User
  { name :: String
  , posts :: Lazy (Array (Lazy Post))
  }
derive instance newtypeUser :: Newtype User _
instance showUser :: Show User where
  show (User x) = "User { name: " <> show x.name <> ", posts }"

newtype Post = Post
  { id :: String
  , author :: Lazy User
  -- , comments :: Lazy (Array Comment)
  }
derive instance newtypePost :: Newtype Post _
instance showPost :: Show Post where
  show (Post x) = "Post { id: " <> show x.id <> ", author }"

wenbo :: Lazy User
wenbo = defer \_ -> User
  { name : "wenbo"
  , posts : defer \_ -> [post1]
  }

post1 :: Lazy Post
post1 = defer \_ -> Post
  { id : "001"
  , author : wenbo
  }

-- | Regular Language
newtype Simple = Simple
  { value :: Int
  , self :: Lazy Simple
  }
derive instance newtypeSimple :: Newtype Simple _
instance showSimple :: Show Simple where
  show (Simple r) = "Simple { value: " <> show r.value <> ", self: ABYSS }"

simple :: Lazy Simple
simple = defer \_ -> Simple
  { value : 0
  , self : simple
  }

main :: Effect Unit
main = do
  logShow $ L.take 3 $ incrementalSequence 0

  logShow $ roundTrip =<< roundTrip =<< roundTrip wenbo
  -- (Just (defer \_ -> User { name: "wenbo", posts }))

  logShow $ selfLoop <<< selfLoop <<< selfLoop $ simple


  where
    roundTrip :: Lazy User -> Maybe (Lazy User)
    roundTrip user = do
      head <- A.head <<< force <<< _.posts <<< unwrap <<< force $ user
      pure <<< _.author <<< unwrap <<< force $ head

    selfLoop :: Lazy Simple -> Lazy Simple
    selfLoop s = _.self <<< unwrap <<< force $ s
