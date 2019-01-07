module Test.Main where

import Generic.GraphQL
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Generic.EnumToDescriptionRow (class EnumToDescriptionRow)
import Generic.IsEnum (class IsEnumPred)
import Generic.RecordToDescriptionRow (class RecordToDescriptionRow)
import Type.Data.Boolean as Bool
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))

-- | IsEnumPred

isEnumPred :: forall a rep b. Generic a rep => IsEnumPred rep b => Proxy a -> Bool.BProxy b
isEnumPred _ = Bool.BProxy :: Bool.BProxy b

data CardType
  = AmericanExpress
  | Discover
  | MasterCard
  | Visa
derive instance genericCardType :: Generic CardType _

isEnumPredExample1 :: Bool.BProxy Bool.True
isEnumPredExample1 = isEnumPred (Proxy :: Proxy CardType)

data List a
  = Nil
  | Cons a (List a)
derive instance genericList :: Generic (List a) _

isEnumPredExample2 :: Bool.BProxy Bool.False
isEnumPredExample2 = isEnumPred (Proxy :: Proxy (List Int))

-- | EnumToDescriptionRow

enumToDescriptionRow :: forall i rep row. Generic i rep => EnumToDescriptionRow rep row => Proxy i -> RProxy row
enumToDescriptionRow _ = RProxy :: RProxy row

enumToDescriptionRowExample1 :: RProxy
  ( "AmericanExpress" :: Maybe String
  , "Discover" :: Maybe String
  , "MasterCard" :: Maybe String
  , "Visa" :: Maybe String
  )
enumToDescriptionRowExample1 = enumToDescriptionRow (Proxy :: Proxy CardType)

-- | RecordToDescriptionRow

recordToDescriptionRow :: forall i rep row. Generic i rep => RecordToDescriptionRow rep row => Proxy i -> RProxy row
recordToDescriptionRow _ = RProxy :: RProxy row

newtype Card = Card
  { id :: String
  , cardholder :: Cardholder -- one-one edge
  }
newtype Cardholder = Cardholder
  { id :: String
  , cards :: Array Card -- one-many edge, from Cardholder to Card
  }
derive instance genericCardholder :: Generic Cardholder _

recordToDescriptionRowExample1 :: RProxy
  ( cards :: Maybe String
  , id :: Maybe String
  )
recordToDescriptionRowExample1 = recordToDescriptionRow (Proxy :: Proxy Cardholder)

-- | GraphQLDescription


main :: Effect Unit
main = do
  log "You should add some tests."
