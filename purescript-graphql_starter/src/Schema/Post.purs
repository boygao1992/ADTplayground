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
    , content:
        G.inputField
          (G.nonNull G.string)
          (Just "The content of the post.")
    }

postType :: G.ObjectType Context (Maybe Post)
postType =
  G.objectType
    "Post"
    (Just "A persisted post.")
    { id:
        G.field'
          (G.nonNull G.id)
          (Just "A unique identity for the post.")
          (\post _ -> pure post.title)
    , title:
        G.field'
          (G.nonNull G.string)
          (Just "The title of the post.")
          (\post _ -> pure post.title)
    , content:
        G.field'
          (G.nonNull G.string)
          (Just "The content of the post.")
          (\post _ -> pure post.content)
    }

data PostAction
  = PostUpdateTitle { title :: String }
  | PostUpdateContent { content :: String }

type PostActionObject =
  { updateTitle :: Maybe { title :: String }
  , updateContent :: Maybe { content :: String }
  }

-- reducer
updateWithPostAction :: Post -> PostAction -> Post
updateWithPostAction post action= case action of
  PostUpdateTitle { title } ->
    post { title = title }
  PostUpdateContent { content } ->
    post { content = content }

-- translator: from GraphQL (Object/Product) to PS (Union)
-- possibly dispatch multiple independent actions (by horizontal partition on state vector)
toPostAction :: PostActionObject -> Maybe PostAction
toPostAction { updateTitle, updateContent } = case updateTitle, updateContent of
  Just arg, Nothing -> Just (PostUpdateTitle arg)
  Nothing, Just arg -> Just (PostUpdateContent arg)
  _, _ -> Nothing

-- PostAction case 1: PostUpdateTitle
postUpdateTitleType :: G.InputObjectType (Maybe { title :: String })
postUpdateTitleType =
  G.inputObjectType
    "PostUpdateTitle"
    (Just "Payload for updating the title of a post.")
    { title:
        G.inputField
          (G.nonNull G.string)
          (Just "New title")
    }

-- PostAction case 2: PostUpdateContent
postUpdateContentType :: G.InputObjectType (Maybe { content :: String })
postUpdateContentType =
  G.inputObjectType
    "PostUpdateContent"
    (Just "Payload for updating the content of a post.")
    { content:
        G.inputField
          (G.nonNull G.string)
          (Just "New content")
    }

-- PostAction
postActionType :: G.InputObjectType (Maybe PostActionObject)
postActionType =
  G.inputObjectType
    "PostAction"
    (Just "Possible actions on a post.")
    { updateTitle:
        G.inputField
          postUpdateTitleType
          (Just "Update the title of a post.")
    , updateContent:
        G.inputField
          postUpdateContentType
          (Just "Update the content of a post.")
    }
