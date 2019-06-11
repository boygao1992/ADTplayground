module Shopify.Api.Products.Data.PublishedStatus where

import RIO
import Servant

data PublishedStatus
  = Any
  | Published
  | Unpublished
  deriving (Eq, Ord, Show, Enum)
instance ToHttpApiData PublishedStatus where
  toQueryParam Any = "any"
  toQueryParam Published = "published"
  toQueryParam Unpublished = "unpublished"
