module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (log, logShow)
import Simple.JSON as JSON
import Foreign (Foreign)
import Foreign as Foreign
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

-- | main
testJSON :: String
testJSON =  " { \"id\": 0 , \"name\": \"wenbo\" , \"status\": \"robot\"} "

type State' =
  { id :: Int
  , name :: String
  , status :: Status
  }

main :: Effect Unit
main = do
  case (JSON.readJSON testJSON) of
    Right (r :: State') -> do
      assertEqual { expected: 0, actual: r.id }
      assertEqual { expected: "wenbo", actual: r.name }
      assertEqual { expected: Case3 "robot", actual: r.status } -- both Case3 and Case4 has the correct Argument type, but it returns the first match
      logShow r
    Left e -> do
      assertEqual { expected: "failed", actual: show e}
