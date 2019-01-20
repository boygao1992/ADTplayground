module ForumExample.Model where

import Data.Generic.Rep (class Generic)

newtype User = User
  { id :: String
  , posts :: { date :: String } -> Array Post
  , comments :: { limit :: Int} -> Array Comment
  }

derive instance genericUser :: Generic User _

newtype Post = Post
  { id :: String
  , author :: User
  , comments :: { limit :: Int } -> Array Comment
  }

derive instance genericPost :: Generic Post _

newtype Comment = Comment
  { id :: String
  , author :: User
  , post :: Post
  }

derive instance genericComment :: Generic Comment _
