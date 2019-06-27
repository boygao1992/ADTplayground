module Polaris.UI.Api.Endpoint where

import Prelude hiding ((/))
import Routing.Duplex (RouteDuplex', int, optional, prefix, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Data.Generic.Rep (class Generic)

data Endpoint
  = Products
derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Products": "products" / noArgs
  }
