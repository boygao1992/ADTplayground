module Data.Generic.Rep.Read.Enum where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Maybe (Maybe(..))
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

genericReadEnum
  :: forall a rep
  . Generic a rep
  => GenericReadEnum rep
  => String
  -> Maybe a
genericReadEnum = map to <<< genericGenericReadEnum

class GenericReadEnum rep where
  genericGenericReadEnum :: String -> Maybe rep

instance genericReadEnumBaseCase
  :: Symbol.IsSymbol name2
  => GenericReadEnum (Constructor name2 NoArguments)
  where
    genericGenericReadEnum name1 =
      if (name1 == Symbol.reflectSymbol (SProxy :: SProxy name2))
      then
        Just $ Constructor NoArguments
      else
        Nothing

instance genericReadEnumInductionStep
  :: ( GenericReadEnum a
    , GenericReadEnum b
    )
  => GenericReadEnum (Sum a b)
  where
    genericGenericReadEnum name
        = Inl <$> genericGenericReadEnum name
      <|> Inr <$> genericGenericReadEnum name
