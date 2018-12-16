module Schema.Mutation where

import Prelude
import Schema.Post (postActionType, postDraftType, postType, toPostAction, updateWithPostAction)
import Store (createPost, deletePost, readPostById, updatePost)

import Context (Context)
import Data.Array (foldl, mapMaybe)
import Data.Maybe (Maybe(..))
import GraphQL.Type as G

mutationType :: G.ObjectType Context (Maybe Unit)
mutationType =
  G.objectType
  "Mutation"
  (Just "The entry of mutation type.")
  { createPost:
      G.field
        postType
        (Just "Create a new post.")
        { draft:
            G.argument
             (G.nonNull postDraftType)
             (Just "Draft of a new post to be created.")
        }
        \_ { draft } ctx -> createPost draft ctx.store
  , updatePost:
      G.field
        postType
        (Just "Update a post by id.")
        { id:
            G.argument
              (G.nonNull G.id)
              (Just "The Id of the post to be updated.")
        , actions:
            G.argument
              (G.nonNull $ G.list
              $ G.nonNull postActionType
              )
              (Just "A list of actions that should be applied to this post.")
        }
        \_ { id, actions } ctx -> do
          let postActions = mapMaybe toPostAction actions
          post <- readPostById id ctx.store
          case post of
            Nothing -> pure Nothing
            Just p ->
              updatePost (foldl updateWithPostAction p postActions) ctx.store

  , deletePost:
      G.field
        (G.nonNull G.boolean)
        (Just "Delete a post.")
        { id:
            G.argument
              (G.nonNull G.id)
              (Just "The Id of the post to be deleted.")
        }
        \_ { id } ctx -> do
          post <- readPostById id ctx.store
          case post of
            Nothing -> pure false
            Just _ -> true <$ deletePost id ctx.store
  }
