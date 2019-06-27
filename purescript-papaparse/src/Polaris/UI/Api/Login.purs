module Polaris.UI.Api.Login where


import Prelude

import Affjax (Response)
import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

login :: String -> Aff (Response (Either String (Maybe String)))
login hostname =
  (\res -> res
      { body = decodeJson <=< lmap Ajax.printResponseFormatError $ res.body }
  )
  <$>
  Ajax.get
    ResponseFormat.json
    ("/login?shop=" <> hostname)
