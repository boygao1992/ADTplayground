module Examples.ForumExample.Model where

import GraphQL.Type.Internal (Id)
import Data.Generic.Rep (class Generic)

newtype User = User
  { id :: Id
  , posts :: { date :: String } -> Array Post
  , comments :: { limit :: Int } -> Array Comment
  }

derive instance genericUser :: Generic User _

newtype Post = Post
  { id :: Id
  , author :: User
  , comments :: { limit :: Int } -> Array Comment
  }

derive instance genericPost :: Generic Post _

newtype Comment = Comment
  { id :: Id
  , author :: User
  , post :: Post
  }

derive instance genericComment :: Generic Comment _

