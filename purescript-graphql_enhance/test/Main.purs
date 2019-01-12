module Test.Main where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Type.Row (RProxy(..))
import Generic.PartitionRow (class PartitionRow)
import B2CExample.Model (Bank, Card)

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

main :: Effect Unit
main = do
  pure unit
