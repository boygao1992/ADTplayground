module Servant.Client.Util where

import RIO
import Servant
import Servant.Client.Core as Client (Request, RequestF(..))

appendSuffixToPath :: Text -> Request -> Request
appendSuffixToPath p req
  = req { requestPath = requestPath req <> toEncodedUrlPiece p }
