module Generic.Show where

import Prelude
import Generic (class Generic, Argument(..), Constructor(..), NoArguments, NoConstructors, Product(..), Sum(..), from)
import Data.Array (intercalate)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

class GenericShow a where
  genericShow' :: a -> String

class GenericShowArgs a where
  genericShowArgs :: a -> Array String

instance genericShowNoConstructors :: GenericShow NoConstructors where
  genericShow' a = genericShow' a -- cyclic definition. It's fine because, since it doesn't have value-level representation (cannot be referred), it's never gonna get called.

instance genericShowArgsNoArguments :: GenericShowArgs NoArguments where
  genericShowArgs _ = []

instance genericShowArgsArgument :: Show a => GenericShowArgs (Argument a) where
  genericShowArgs (Argument a) = [ show a ]

instance genericShowConstructors
  :: (IsSymbol name, GenericShowArgs a)
  => GenericShow (Constructor name a) where
  genericShow' (Constructor a) =
    let
      constructorName = reflectSymbol (SProxy :: SProxy name)
    in
      case genericShowArgs a of
        [] -> constructorName
        args -> "(" <> constructorName <> " " <> intercalate " " args <> ")"

instance genericShowSum
  :: (GenericShow a, GenericShow b)
  => GenericShow (Sum a b) where
    genericShow' (Inl a) = genericShow' a
    genericShow' (Inr b) = genericShow' b

instance genericShowArgsProduct
  :: (GenericShowArgs a, GenericShowArgs b)
  => GenericShowArgs (Product a b) where
    genericShowArgs (Product a b) = genericShowArgs a <> genericShowArgs b

genericShow :: forall a rep. Generic a rep => GenericShow rep => a -> String
genericShow = genericShow' <<< from
