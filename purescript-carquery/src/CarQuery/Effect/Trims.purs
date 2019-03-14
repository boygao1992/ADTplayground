module CarQuery.Effect.Trims
( normalization
, sampling
) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Lens.Index.Recordable (toRecord)
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Data.String.Pattern (Pattern (..))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Model.Car (Car, fromSource)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

-- Utils

parseFileName :: String -> Maybe String
parseFileName = A.head <<< String.split (Pattern ".")

readTrim :: String -> String -> Effect (Array Car)
readTrim inputBasePath fileName = do
  let filePath = inputBasePath <> fileName
  jsonStr <- FS.readTextFile UTF8 filePath
  case jsonParser jsonStr of
    Left _ -> pure []
    Right json ->
      case decodeJson json of
        Left _ -> pure []
        Right (trims :: { "Trims" :: Array (Object (Maybe String))}) -> do
          let trimList = trims."Trims"
              carList = map (fromSource <<< toRecord) trimList :: Array Car
          pure carList

insertTrim :: String -> Object (Array Car) -> String -> Effect (Object (Array Car))
insertTrim inputBasePath obj fileName = do
  carList <- readTrim inputBasePath fileName
  pure $ Object.insert
            (fromMaybe fileName <<< parseFileName $ fileName)
            carList
            obj


sampleTrim :: Int -> String -> Object (Array Car) -> String -> Effect (Object (Array Car))
sampleTrim sampleNum inputBasePath obj fileName = do
  carList <- readTrim inputBasePath fileName
  pure $ Object.insert
            (fromMaybe fileName <<< parseFileName $ fileName)
            (A.take sampleNum carList)
            obj

-- API

normalization :: String -> String -> Effect Unit
normalization inputBasePath outputCollectionPath = do
  fileNames <- FS.readdir inputBasePath
  -- result :: Object (Array Car)
  result <- foldM reducerM Object.empty fileNames
  FS.writeTextFile UTF8 outputCollectionPath
    <<< stringify
    <<< encodeJson
      $ result
  where
    reducerM :: Object (Array Car) -> String -> Effect (Object (Array Car))
    reducerM = insertTrim inputBasePath

sampling :: Int -> String -> String -> Effect Unit
sampling sampleNum inputBasePath outputCollectionPath = do
  fileNames <- FS.readdir inputBasePath
  -- result :: Object (Array Car)
  result <- foldM reducerM Object.empty fileNames
  FS.writeTextFile UTF8 outputCollectionPath
    <<< stringify
    <<< encodeJson
      $ result

  where
    reducerM :: Object (Array Car) -> String -> Effect (Object (Array Car))
    reducerM = sampleTrim sampleNum inputBasePath
