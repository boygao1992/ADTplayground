module Test.Main where

import Data.Either
import Generic.SumReadPayload
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Generic.EnumToDescriptionRow (class EnumToDescriptionRow)
import Generic.IsEnum (class IsEnumPred)
import Generic.RecordToDescriptionRow (class RecordToDescriptionRow)
import Prim.RowList (kind RowList, Nil, Cons) as RowList
import RowList.Utils (class ReverseRowList)
import RowToFunc (rowToCons)
import Test.Assert (assertEqual)
import Type.Data.Boolean as Bool
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (SProxy(..))
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

-- | RowToFunc
user :: Int -> String -> String -- NOTE RowToList sorts fields in alphabet order
      -> { name :: String
         , id :: String
         , age :: Int
         }
user = rowToCons (RProxy :: RProxy (id :: String, name :: String, age :: Int))

-- | ReverseRowList

reverseRowList :: forall i o. ReverseRowList i o => RLProxy i -> RLProxy o
reverseRowList _ = RLProxy :: RLProxy o

reverseRowListExample :: RLProxy (RowList.Cons "id" String (RowList.Cons "name" String (RowList.Cons "age" Int RowList.Nil)))
reverseRowListExample = reverseRowList (RLProxy :: RLProxy (RowList.Cons "age" Int (RowList.Cons "name" String (RowList.Cons "id" String RowList.Nil))))

-- | sumReadPayload
data PostAction
  = PostUpdateTitle { title :: String }
  | PostUpdateContent { content :: String }
derive instance genericPostAction :: Generic PostAction _
instance eqPostAction :: Eq PostAction where
  eq = genericEq
instance showPostAction :: Show PostAction where
  show = genericShow

shouldEqual :: forall a . Eq a => Show a => a -> a -> Effect Unit
shouldEqual a b =
  assertEqual { actual : a, expected : b }

main :: Effect Unit
main = do
  shouldEqual
    (user 14 "robot00" "wenbo")
    { age: 14, id: "robot00", name: "wenbo" }

  shouldEqual
    ( sumReadPayload
      (SProxy :: SProxy "PostUpdateTitle")
      { title : "hello world" } :: Either String PostAction
    )
    (Right $ PostUpdateTitle { title : "hello world" })

  shouldEqual
    ( toConstructor
      { "PostUpdateTitle" : Nothing
      , "PostUpdateContent" : Just { content : "first todo" }
      } :: Either String PostAction
    )
    (Right $ PostUpdateContent { content : "first todo" })

  shouldEqual
    ( toConstructor
      { "PostUpdateTitle" : Just { title : "hello world" }
      , "PostUpdateContent" : Just { content : "first todo" }
      } :: Either String PostAction
    )
    (Left "More than one fields have payloads")

  shouldEqual
    ( toConstructor
      { "PostUpdateTitle" : Nothing
      , "PostUpdateContent" : Nothing
      } :: Either String PostAction
    )
    (Left "None of the fields has payload")

