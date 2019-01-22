module GraphQL.Type.Internal.ToInputObject where

import GraphQL.Type.Internal (InputObjectType)

class ToInputObject (i :: Type) (o :: Type) | i -> o


{- input

newtype PostDraft = PostDraft
  { author :: { id :: Id
              , name :: String
              }
  , content :: { date :: String
               , lines :: Int
               , todoList :: Array
                               { id :: Id
                               , todo :: String
                               }
               }
  }

-}

{- output

G.input
{ name: "PostDraft"
, fields:
  { author:
    { type: G.input
            { name: "PostDraft_author"
            , fields:
              { id: { type: G.id }
              , name: { type: G.string }
              }
            }
    }
  , content:
    { type: G.input
            { name: "PostDraft_content"
            , fields:
              { date: { type: G.string }
              , lines: { type: G.int }
              , todoList: { type: G.list
                                  ( G.input
                                    { name: "PostDraft_content_todoList-Item"
                                    , fields:
                                      { id: { type: G.id }
                                      , todo: { type: G.string }
                                      }
                                    }
                                  )
                          }
              }
            }
    }
  }
}

-}
