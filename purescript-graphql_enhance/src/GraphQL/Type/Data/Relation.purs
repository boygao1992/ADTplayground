module GraphQL.Type.Data.Relation  where

import Data.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)

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
