module Schema.Post where

import Prelude

import Context (Context)

import Data.Maybe (Maybe(..))
import GraphQL.Type as G
import Store (PostDraft, Post)

postDraftType :: G.InputObjectType (Maybe PostDraft)
postDraftType =
  G.inputObjectType
    "PostDraft"
    (Just "A post yet to be persisted.")
    { title:
        G.inputField
          (G.nonNull G.string)
          (Just "The title of the post.")
    , context:
        G.inputField
          (G.nonNull G.string)
          (Just "The context of the post.")
    }
