module ForumExample.Model where

newtype User = User
  { id :: String
  , posts :: { date :: String } -> Array Post
  , comments :: { limit :: Int} -> Array Comment
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
