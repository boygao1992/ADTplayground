module Test.Main where

import Prelude

import B2CExample.Model (Bank, Card)
import Data.Maybe (Maybe)
import Effect (Effect)
import Generic.PartitionRow (class PartitionRow)
import Type.Data.Boolean (BProxy(..), True, False)
import Type.Data.Symbol (SProxy(..))
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import Type.Row.Utils (class HasFieldPred, class IsSubsetPred)

-- | PartitionRow
partitionRow
  :: forall row scalars relations
   . PartitionRow row scalars relations
  => RProxy row
  -> { scalars :: RProxy scalars
     , relations :: RProxy relations
     }
partitionRow _ =
  { scalars : RProxy :: RProxy scalars
  , relations : RProxy :: RProxy relations
  }

partitionRowExample ::
  { scalars :: RProxy
               ( id :: String
               , name :: Maybe String
               , age :: Maybe Int
               )
  , relations :: RProxy
                 ( banks :: Array Bank
                 , cards :: Array Card
                 )
  }
partitionRowExample =
  partitionRow
  $ RProxy :: RProxy
    ( id :: String
    , name :: Maybe String
    , age :: Maybe Int
    , cards :: Array Card
    , banks :: Array Bank
    )

-- | HasFieldPred

-- Test
hasFieldPred :: forall row name b. HasFieldPred row name b => RProxy row -> SProxy name -> BProxy b
hasFieldPred _ _ = BProxy :: BProxy b

hasFieldPredExample1 :: BProxy True
hasFieldPredExample1 = hasFieldPred
                      (RProxy :: RProxy
                                 ( description :: String )
                      )
                      (SProxy :: SProxy "description")

hasFieldPredExample2 :: BProxy False
hasFieldPredExample2 = hasFieldPred
                      (RProxy :: RProxy
                                 ( description :: String )
                      )
                      (SProxy :: SProxy "resolve")

-- Test
isSubset :: forall hypo hyper b. IsSubsetPred hypo hyper b => RProxy hypo -> RProxy hyper -> BProxy b
isSubset _ _ = BProxy :: BProxy b

isSubsetExample1 :: BProxy True
isSubsetExample1 = isSubset
                    (RProxy :: RProxy (id :: String))
                    (RProxy :: RProxy (id :: String, name :: String))

isSubsetExample2 :: BProxy True
isSubsetExample2 = isSubset
                    (RProxy :: RProxy (id :: String))
                    (RProxy :: RProxy (id :: String))

isSubsetExample3 :: BProxy False
isSubsetExample3 = isSubset
                    (RProxy :: RProxy (id :: String))
                    (RProxy :: RProxy (id :: Int, name :: String))

isSubsetExample4 :: BProxy False
isSubsetExample4 = isSubset
                    (RProxy :: RProxy (id :: String))
                    (RProxy :: RProxy ())

-- | experiment for open record
-- e.g. forall r. { id :: String | r}
--   -> { id :: String }

type Func1 r = { id :: String | r } -> String

func1 :: forall r. Func1 r
func1 = _.id

main :: Effect Unit
main = do
  pure unit
