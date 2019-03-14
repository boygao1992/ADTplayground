module CarDatabase.GetMakes where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..), either)
import Data.String.CodeUnits (drop, dropRight) as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS

type Response =
  { result :: Array Make
  }
type Make =
  { make_id :: String
  , make :: String
  }

getMakes :: Aff (Array Make)
getMakes = do
  res <- AX.get ResponseFormat.string url
  case res.body of
    Left error -> pure []
    Right body -> do
      pure $ either (const []) identity $ do
        json <- jsonParser body
        (response :: Response) <- decodeJson json
        pure $ response.result
  where
    url = "https://databases.one/api/?format=json&select=make&api_key=Your_Database_Api_Key"

getMakes' :: Aff (Array Make)
getMakes' = do
  body <- liftEffect $ FS.readTextFile UTF8 filePath
  pure $ either (const []) identity $ do
    json <- jsonParser body
    (response :: Response) <- decodeJson json
    pure $ response.result
  where
    filePath = "./data/CarDatabase/all_makes.json"
