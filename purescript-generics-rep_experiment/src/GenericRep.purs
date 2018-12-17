module Generic where

import Prelude
import Data.Maybe (Maybe(..))

-- | no data constructor, type only
data NoConstructors

-- | data constructor with no arguments
data NoArguments = NoArguments

-- | sum type: multiple data constructors, associated to the same type
data Sum a b = Inl a | Inr b

-- | product type: data constructor with multiple arguments (bundling multiple types into a single type)
data Product a b = Product a b

-- | data constructor with its name carried in its type as a type-level literal
newtype Constructor (name :: Symbol) a = Constructor a

-- | an argument (type) in a data constructor
newtype Argument a = Argument a

-- | functional dependency, a -> rep :
-- |   given a type `a`, there is only one unique type `rep` associated with `a` under type class `Generic`
-- |   if instance (Generic a rep1) is defined, defining another instance (Generic a rep2) will raise a type error
-- |   (directional one-to-one correspondence, not the other way around)
-- |   (if in both directions, then `a` and `rep` are isomorphic)
-- |   (rep -> a is not necessary here because we only use Generic as an intermediate encoding to transform data from one format to another, but never use Generic standalone or as canonical representation)
-- | without compiler's help, instance is not derivable
-- | source:  purescript/src/Language/PureScript/Sugar/TypeClasses/Deriving.hs
class Generic a rep | a -> rep where
  -- | encode any type as a unified representation
  from :: a -> rep
  -- | decode
  to :: rep -> a

instance genericMaybe
  :: Generic (Maybe a) (Sum (Constructor "Nothing" NoArguments)
                            (Constructor "Just" (Argument a))) where
  from Nothing = Inl (Constructor NoArguments)
  from (Just a) = Inr (Constructor (Argument a))

  to (Inl _) = Nothing
  to (Inr (Constructor (Argument a))) = Just a
