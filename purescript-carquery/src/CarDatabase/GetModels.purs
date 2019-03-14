module CarDatabase.GetModels where


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
