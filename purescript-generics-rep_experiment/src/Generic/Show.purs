module Generic.Show where

import Prelude

class GenericShow a where
  genericShow' :: a -> String

class GenericShowArgs a where
  genericShowArgs :: a -> Array String
