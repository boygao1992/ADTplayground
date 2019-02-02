module Examples.ForumExample.Schema where

import Prelude

import Examples.ForumExample.Query (Query, query)
import GraphQL.Type.Internal (Schema)
import GraphQL.Type.Internal (schema) as G

schema :: Schema Query Unit
schema = G.schema query
