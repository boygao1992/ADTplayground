module Examples.PostExample.Model where

import Data.Maybe (Maybe(..))

newtype Id = Id String

newtype PostDraft = PostDraft -- InputObjectType
  { title :: String
  , content :: String
  }

newtype Post = Post -- ObjectType
  { id :: Id
  , title :: String
  , content :: String
  }

-- data PostAction
--   = PostUpdateTitle { title :: String }
--   | PostUpdateContent { content :: String }
-- instance typeNamePostAction :: TypeName PostAction "PostAction"

{-
  G.inputObjectType "PostAction" Nothing
  { postUpdateTitle :
    G.inputObjectType "PostUpdateTitle" Nothing
    { title :
      G.inputField
        (G.nonnull G.string) Nothing
    }
  , postUpdateContent :
    G.inputObjectType "PostUpdateContent" Nothing
    { content :
      G.inputField
        (G.nonnull G.string) Nothing
    }
  }

  toConstructor :: Generic PostAction rep =>
    { postUpdateTitle :: Maybe { title :: String }
    , postUpdateContent :: Maybe { content :: String }
    } -> Either String PostAction
  toConstructor inputObject = case Row.uncons inputObject of
    Nothing ->
      Left "invalid inputObject"
    Just name value restInputObject -> case value of
      Nothing ->
        toConstructor restInputObject
      Just payload ->
        Right $ SumReadPayload name value
-}

newtype PostAction = PostAction -- InputObjectType
  { updateTitle :: Maybe PostUpdateTitle
  , updateContent :: Maybe PostUpdateContent
  }
newtype PostUpdateTitle = PostUpdateTitle { title :: String } -- InputObjectType
newtype PostUpdateContent = PostUpdateContent { content :: String } -- InputObjectType

newtype Query = Query -- ObjectType
  { post :: { id :: Id } -> Post -- with args
  , posts :: Array Post -- no args
  }

newtype Mutation = Mutation -- ObjectType
  { createPost :: { draft :: PostDraft } -> Post
  , updatePost :: { id :: Id, actions :: Array PostAction } -> Post
  , deletePost :: { id :: Id } -> Boolean
  }
