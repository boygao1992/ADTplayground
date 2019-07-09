module Polaris.UI.Api.Request where

import Prelude

import Affjax (Request, printResponseFormatError, request)
import Affjax.RequestBody as ARB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ARF
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Routing.Duplex (print)

import Polaris.UI.Api.Endpoint (Endpoint, endpointCodec)

newtype Token = Token String
derive newtype instance eqToken :: Eq Token
derive newtype instance ordToken :: Ord Token
derive instance newtypeToken :: Newtype Token _
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

newtype BaseUrl = BaseUrl String
derive newtype instance eqBaseUrl :: Eq BaseUrl
derive newtype instance ordBaseUrl :: Ord BaseUrl
derive instance newtypeBaseUrl :: Newtype BaseUrl _
derive instance genericBaseUrl :: Generic BaseUrl _
instance showBaseUrl :: Show BaseUrl where
  show = genericShow

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseUrl -> RequestOptions -> Request Json
defaultRequest (BaseUrl baseUrl) { endpoint, method: reqMethod } =
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
  , headers: []
  , content: ARB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: ARF.json
  }
  where
    method /\ body = case reqMethod of
      Get -> GET /\ Nothing
      Post body -> POST /\ body
      Put body -> PUT /\ body
      Delete -> DELETE /\ Nothing

