module Schema where

import Prelude

import GraphQL.Type as G
import Context (Context)
import Schema.Query (queryType)
import Data.Maybe (Maybe(..))

schema :: G.Schema Context Unit
schema = G.schema queryType Nothing
