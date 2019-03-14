module CarQuery.Api.GetMakes where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..), either)
import Data.String.CodeUnits (drop, dropRight) as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS

type Response =
  { "Makes" :: Array Make
  }

type Make =
  { make_id :: String
  , make_display :: String
  , make_is_common :: String
  , make_country :: String
  }

-- getMakes :: Aff (Either String (Array Make))
getMakes :: Aff (Array Make)
getMakes = do
  res <- AX.get ResponseFormat.string url
  pure $ case res.body of
    Left error -> []
    Right body -> either (const []) identity do
      let jsonStr = String.drop 2 <<< String.dropRight 2 $ body
      json <- jsonParser jsonStr
      (response :: Response) <- decodeJson json
      pure $ response."Makes"
  where
    url = "https://www.carqueryapi.com/api/0.3/?callback=?&cmd=getMakes"

getMakes' :: Aff (Array Make)
getMakes' = do
  body <- liftEffect $ FS.readTextFile UTF8 filePath
  pure $ either (const []) identity $ do
    json <- jsonParser body
    (response :: Array Make) <- decodeJson json
    pure $ response
  where
    filePath = "./data/CarQuery/all_makes.json"
