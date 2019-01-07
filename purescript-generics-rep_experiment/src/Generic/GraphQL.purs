module Generic.GraphQL where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Generic.EnumToDescriptionRow (class EnumToDescriptionRow)
import Generic.IsEnum (class IsEnumPred)
import Generic.RecordToDescriptionRow (class RecordToDescriptionRow)
import Type.Data.Boolean as Bool
import Type.Proxy (Proxy)

class TypeToDescriptionRow rep (row :: # Type) | rep -> row

instance typeToDescriptionRowIfEnum ::
  ( IsEnumPred rep b
  ) => TypeToDescriptionRow rep row

class TypeToDescriptionRowIfEnum (b :: Bool.Boolean) rep (row :: # Type) | b rep -> row

instance typeToDescriptionRowIsEnum ::
  ( EnumToDescriptionRow rep row
  ) => TypeToDescriptionRowIfEnum Bool.True rep row
else instance typeToDescriptionRowIsRecord :: -- TODO may add Union
  ( RecordToDescriptionRow rep row
  ) => TypeToDescriptionRowIfEnum Bool.False rep row


-- class (TypeToDescriptionRow rep row, Generic a rep) <= GraphQLDescription a rep row | a -> rep, rep -> row where
--   description :: Record row

-- class GraphQLDescription a where
--   description :: forall rep row. Generic a rep => RecordToDescriptionRow rep row => Record row

data InputObjectType a

type InputObjectTypeConstructor = forall a rep desRow. Generic a rep => TypeToDescriptionRow rep desRow => Proxy a -> Record desRow -> InputObjectType (Maybe a)

type InputObjectTypeNoDescriptionConstructor = forall a rep. Generic a rep => Proxy a -> InputObjectType (Maybe a)
