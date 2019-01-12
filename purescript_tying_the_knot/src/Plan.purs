module Plan where

import Prelude

import Data.Maybe (Maybe(..))
import GraphQL.Type as G

newtype User = User
  { name :: String
  , posts :: Array Post
  }

newtype Post = Post
  { id :: String
  , author :: User
  }

{-
https://stackoverflow.com/questions/46770501/graphql-non-nullable-array-list

declaration accepts: | null | []   | [null] | [{foo: 'BAR'}]
------------------------------------------------------------------------
[Vote!]!             | no   | yes  | no     | yes
[Vote]!              | no   | yes  | yes    | yes
[Vote!]              | yes  | yes  | no     | yes
[Vote]               | yes  | yes  | yes    | yes

irrational API design would possibly lead to a list that is not [Vote!]!

none of the cases delivers relational semantics.
-}

{-
GraphQL representation

userType :: forall ctx. G.ObjectType ctx (Maybe User)
userType =
  G.objectType "User" Nothing $ \_ ->
  { name :
    G.field'
      (G.nonNull G.string) -- :: ScalarType String
      Nothing
      (\user _ -> pure user.name) -- { name :: String } -> ctx -> Aff String
  , posts :
    G.field
      -- NOTE List is always nonNull since itself already has a Monoid instance,
      --   mempty = [] :: forall a. Array a
      -- for Maybe,
      --   mempty = Nothing :: forall a. Maybe a
      (G.nonNull $ G.List $ G.nonNull postType) -- :: ObjectType ctx (Array Post)
      Nothing
      (\user _ -> getPosts user.name)
  }

postType :: forall ctx. G.ObjectType ctx (Maybe Post)
postType =
  G.objectType "Post" Nothing $ \_ ->
  { id :
    G.field'
      (G.nonNull G.id) -- :: ScalarType String
      Nothing
      (\post _ -> pure post.id) -- { id :: String } -> ctx -> Aff String
  , author :
    G.field
      (G.nonNull userType) -- :: ObjectType ctx User
      Nothing
      (\post _ -> getUser post.id)

  }

-}
