module GraphQL.Type where

import Prelude

import Data.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)
import Type.Data.Boolean as Bool

data ObjectType psType

newtype Id = Id String

-- | IsUnitPred
class IsUnitPred a (b :: Bool.Boolean) | a -> b

instance isUnitPredYes :: IsUnitPred Unit Bool.True
else instance isUnitPredNo :: IsUnitPred a Bool.False

-- | IsScalarPred
class IsScalarPred a (b :: Bool.Boolean) | a -> b

instance isScalarPredInt :: IsScalarPred Int Bool.True
else instance isScalarPredNumber :: IsScalarPred Number Bool.True
else instance isScalarPredString :: IsScalarPred String Bool.True
else instance isScalarPredId :: IsScalarPred Id Bool.True
else instance isScalarPredBoolean :: IsScalarPred Boolean Bool.True
else instance isScalarPredNo :: IsScalarPred a Bool.False

{- mapping
Maybe a -> a
a -> NotNull a
Array a -> NotNull List (NotNull a)
-}

foreign import kind Relation
foreign import data ExactOne :: Relation -- a
foreign import data ZeroOrOne :: Relation -- Maybe a
foreign import data ZeroOrMore :: Relation -- Array a
foreign import data OneOrMore :: Relation -- NonEmpty Array a

-- | ParseRelation
class ParseRelation typ (rela :: Relation) a | typ -> rela a, rela a -> typ

instance parseTypeOneOrMore :: ParseRelation (NonEmpty Array a) OneOrMore a
else instance parseTypeZeroOrMore :: ParseRelation (Array a) ZeroOrMore a
else instance parseTypeZeroOrOne :: ParseRelation (Maybe a) ZeroOrOne a
else instance parseTypeExactOne :: ParseRelation a ExactOne a
