module Schema.Message where

import Prelude

import Context (Context)
import Data.Maybe (Maybe(..), fromMaybe)
import GraphQL.Type as G
import Store (Message)

messageType :: G.ObjectType Context (Maybe Message)
messageType =
  G.objectType
    "Message"
    (Just "A meaningless piece of noise in the mountain of digital dumpyard.")
    { id:
        G.field'
          (G.nonNull G.id)
          (Just "A unique identifier for the message.")
          (\msg _ -> pure msg.id)
    , name:
        G.field'
          (G.nonNull G.string)
          Nothing
          (\msg _ -> pure $ fromMaybe "default message" msg.content)
    }
