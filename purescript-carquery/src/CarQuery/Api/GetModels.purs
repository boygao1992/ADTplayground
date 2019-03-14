module CarQuery.Api.GetModels where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.String.CodeUnits (drop, dropRight) as String
import Effect.Aff (Aff)

type Model =
  { model_name :: String
  , model_make_id :: String
  }

type Options =
  { make :: String -- Vehicle Brand/Make
  , year :: Int
  , sold_in_us :: Boolean
  , body :: String -- Vehicle Type
  }

-- https://www.carqueryapi.com/api/0.3/?callback=?&cmd=getModels&make=ford&year=2005&sold_in_us=1&body=SUV
getModels :: String -> Aff (Either String (Array Model))
getModels make = do
  res <- AX.get ResponseFormat.string url
  pure $ case res.body of
    Left error -> Left $ ResponseFormat.printResponseFormatError error
    Right body -> do
      let jsonStr = String.drop 2 <<< String.dropRight 2 $ body
      json <- jsonParser jsonStr
      ({ "Models": modelList } :: { "Models" :: Array Model }) <- decodeJson json
      pure $ modelList
  where
    url = "https://www.carqueryapi.com/api/0.3/?callback=?&cmd=getModels&make=" <>  make
