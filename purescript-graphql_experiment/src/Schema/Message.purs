module Schema.Message where

import Prelude

import Context (Context)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import GraphQL.Type as G
import Store (Message, readMessageById)

messageType :: G.ObjectType Context (Maybe Message)
messageType =
  G.objectTypeRec
    "Message"
    (Just "A meaningless piece of noise in the mountain of digital dumpyard.")
    ( \_ -> { id:
              G.field'
                (G.nonNull G.id)
                (Just "A unique identifier for the message.")
                (\msg _ -> pure msg.id)
            -- , comments:
            --   G.field'
            --     (G.nonNull $ G.list $ G.nonNull G.string)
            --     (Just "A list of comments to the message.")
            --     (\msg _ -> pure msg.comments)
            -- , replys:
            --   G.field'
            --     messageType
            --     Nothing
            --     (\({ comments } { store }) -> do
            --         map catMaybes $ sequence $ map (readMessageById store) comments
            --     )
            }
    )
