module Polaris.UI.Api.Product where

import Prelude

import Affjax (Response)
import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Aff (Aff)
import Data.Argonaut.Decode (decodeJson)

import Polaris.Data.Product (Products)

getProducts :: String -> Aff (Response (Either String Products))
getProducts hostname =
  (\res -> res
      { body = decodeJson <=< lmap Ajax.printResponseFormatError $ res.body }
  )
  <$>
  Ajax.get
    ResponseFormat.json
    ("/products?shop=" <> hostname)
