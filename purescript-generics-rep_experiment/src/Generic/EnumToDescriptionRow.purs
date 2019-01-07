module Generic.EnumToDescriptionRow where

import Data.Generic.Rep (Constructor, NoArguments, Sum)
import Data.Maybe (Maybe)
import Type.Row as Row

class EnumToDescriptionRow rep (row :: # Type) | rep -> row

instance enumToRowBaseCase ::
  ( Row.Cons name (Maybe String) () row
  ) => EnumToDescriptionRow (Constructor name NoArguments) row
else instance enumToRowInductionStep ::
  ( EnumToDescriptionRow r restRow
  , Row.Cons name (Maybe String) restRow row
  ) => EnumToDescriptionRow (Sum (Constructor name NoArguments) r) row
