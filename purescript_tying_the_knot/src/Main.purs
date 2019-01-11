module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype User = User
  { name :: String
  , posts :: Ref (Array Post)
  }

newtype Post = Post
  { id :: String
  , author :: Ref (Maybe User)
  }

main :: Effect Unit
main = do
  posts <- Ref.new []
  author <- Ref.new Nothing

  let wenbo = User { name : "wenbo", posts }
      post1 = Post { id : "001", author }
  Ref.modify_ (const $ Just wenbo) author
  Ref.modify_ (const $ [ post1 ]) posts

