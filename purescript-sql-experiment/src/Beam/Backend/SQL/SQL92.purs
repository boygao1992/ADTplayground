module Beam.Backend.SQL.SQL92 where

import Prelude

class HasSqlValueSyntax expr ty where
  sqlValueSyntax :: ty -> expr

autoSqlValueSyntax :: forall a expr. HasSqlValueSyntax expr String => Show a => a -> expr
autoSqlValueSyntax = sqlValueSyntax <<< show

-- type Sql92SelectExpressionSyntax select
-- type Sql92SelectProjectionSyntax select
-- type Sql92SelectGroupingSyntax select
-- type Sql92SelectFromSyntax select
-- type Sql92InsertExpressionSyntax select
-- type Sql92TableNameSyntax select

-- type Sql92ValueSyntax cmdSyntax
-- type Sql92ExpressionSyntax cmdSyntax
-- type Sql92ExtractFieldSyntax cmdSyntax
-- type Sql92HasValueSyntax cmdSyntax

-- type Sql92SelectSanityCheck select

-- type Sql92ReasonableMarshaller be

-- | Type classes for syntaxes which can be displayed
class Sql92DisplaySyntax syntax where

  -- | Render the syntax as a 'String', representing the SQL expression it
  -- stands for
  displaySyntax :: syntax -> String

class Sql92SelectSyntax cmd o | cmd -> o
class Sql92InsertSyntax cmd o | cmd -> o
class Sql92UpdateSyntax cmd o | cmd -> o
class Sql92DeleteSyntax cmd o | cmd -> o

class
  -- ( IsSql92SelectSyntax (Sql92SelectSyntax cmd)
  -- , IsSql92InsertSyntax (Sql92InsertSyntax cmd)
  -- , IsSql92UpdateSyntax (Sql92UpdateSyntax cmd)
  -- , IsSql92DeleteSyntax (Sql92DeleteSyntax cmd)
  -- ) <=
  IsSql92Syntax cmd where

  selectCmd :: forall o. Sql92SelectSyntax cmd o => o -> cmd
  insertCmd :: forall o. Sql92InsertSyntax cmd o => o -> cmd
  updateCmd :: forall o. Sql92UpdateSyntax cmd o => o -> cmd
  deleteCmd :: forall o. Sql92DeleteSyntax cmd o => o -> cmd


