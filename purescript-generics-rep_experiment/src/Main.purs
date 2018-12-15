module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Types.Generic.Rep (Encoding, defaultEncoding)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (log, logShow)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON as JSON
import Test.Assert (assertEqual)

-- | Utils
class UntaggedSumRep rep where
  untaggedSumRep :: Foreign -> Foreign.F rep

instance untaggedSumRepSum ::
  ( UntaggedSumRep a
  , UntaggedSumRep b
  ) => UntaggedSumRep (GR.Sum a b) where
    untaggedSumRep f
        = GR.Inl <$> untaggedSumRep f
      <|> GR.Inr <$> untaggedSumRep f

instance untaggedSumRepConstructor ::
  UntaggedSumRep a => UntaggedSumRep (GR.Constructor name a) where
    untaggedSumRep f = GR.Constructor <$> untaggedSumRep f

instance untaggedSumRepArgument ::
  JSON.ReadForeign a => UntaggedSumRep (GR.Argument a) where
    untaggedSumRep f = GR.Argument <$> JSON.readImpl f

-- | Status, sum type
data Status
  = Case1 Int
  | Case2 Boolean
  | Case3 String
  | Case4 String

derive instance genericStatus :: GR.Generic Status _
instance eqStatus :: Eq Status where
  eq = genericEq
instance showStatus :: Show Status where
  show = genericShow
instance decodeJsonStatus :: DecodeJson Status where
  decodeJson = genericDecodeJson
instance encodeJsonStatus :: EncodeJson Status where
  encodeJson = genericEncodeJson

instance readForeignStatus :: JSON.ReadForeign Status where
  readImpl f = GR.to <$> untaggedSumRep f


-- | State, product type
newtype State = State
  { id :: Int
  , name :: String
  , status :: Status
  }

derive instance genericState :: GR.Generic State _
instance showState :: Show State where
  show = genericShow

instance readForeignState :: JSON.ReadForeign State where
  readImpl f = GR.to <$> untaggedSumRep f

instance decodeJsonState :: DecodeJson State where
  decodeJson = genericDecodeJson

instance encodeJsonState :: EncodeJson State where
  encodeJson = genericEncodeJson

-- | main
testJSON :: String
testJSON =  " { \"id\": 0 , \"name\": \"wenbo\" , \"status\": \"robot\"} "

testObject :: State
testObject = State
  { id : 0
  , name : "wenbo"
  , status : Case3 "robot"
  }

defaultEncodingOfTestObject :: String
defaultEncodingOfTestObject = """
{"values":[{"status":{"values":["robot"],"tag":"Case3"},"name":"wenbo","id":0}],"tag":"State"}
"""

main :: Effect Unit
main = do
  case (JSON.readJSON testJSON) of
    Right ((State r) :: State) -> do
      assertEqual { expected: 0, actual: r.id }
      assertEqual { expected: "wenbo", actual: r.name }
      assertEqual { expected: Case3 "robot", actual: r.status } -- both Case3 and Case4 has the correct Argument type, but it returns the first match
      logShow r
    Left e -> do
      assertEqual { expected: "failed", actual: show e}

  -- case (jsonParser testJSON) of
  --   Left e ->
  --     log e
  --   Right json -> do
  --     logShow $ (decodeJson json) :: Either String State

  log $ stringify $ encodeJson testObject
