module Test.CollectEntities where

import Type.Data.List (Cons, LProxy(..), Nil)
import GraphQL.Type.Internal.RootObject (class CollectEntities)
import Type.Proxy (Proxy (..))
import Examples.ForumExample.Model (Comment, Post, User)

collect :: forall spec entities. CollectEntities spec entities => Proxy spec -> LProxy entities
collect _ = LProxy :: LProxy entities

collectTest :: LProxy (Cons Comment (Cons Post (Cons User Nil)))
collectTest =
  collect (Proxy :: Proxy User)
