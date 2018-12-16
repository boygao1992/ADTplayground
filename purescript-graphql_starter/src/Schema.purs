module Schema where

import Prelude

import GraphQL.Type as G

import Context (Context)
import Schema.Query (queryType)
import Schema.Mutation (mutationType)

schema :: G.Schema Context Unit
schema = G.schema queryType $ pure mutationType
