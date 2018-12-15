module Store where

import Prelude

import Data.Array ((:), filter)
import Data.Foldable (class Foldable, find)
import Data.Maybe (Maybe)
import Data.UUID (genUUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

type Id = String

type Post =
  { id :: Id
  , title :: String
  , context :: String
  }

type PostDraft =
  { title :: String
  , context :: String
  }

type Store =
  { posts :: Ref.Ref (Array Post) }

createStore :: Aff Store
createStore = do
  posts <- liftEffect $ Ref.new []
  pure { posts }

findById
  :: forall r f
   . Foldable f
  => Id
  -> f { id :: Id | r }
  -> Maybe { id :: Id | r}
findById id = find ( (_ == id) <<< _.id )

createPost :: PostDraft -> Store -> Aff (Maybe Post)
createPost ({ title, context }) ({ posts }) = liftEffect do
  id <- show <$> genUUID
  let post = { id, title, context }
  Ref.modify_ (post : _) posts
  pure $ pure post

readPosts :: Store -> Aff (Array Post)
readPosts = liftEffect <<< Ref.read <<< _.posts

readPostById :: Id -> Store -> Aff (Maybe Post)
readPostById id = map (findById id) <<< readPosts

updatePost :: Post -> Store -> Aff Unit
updatePost newPost ({ posts }) =
  liftEffect
  $ Ref.modify_
      (map (\post -> if post.id == newPost.id then newPost else post))
      posts

deletePost :: Id -> Store -> Aff Unit
deletePost id ({ posts }) =
  liftEffect $ Ref.modify_ (filter (not (_ == id) <<< _.id )) posts
