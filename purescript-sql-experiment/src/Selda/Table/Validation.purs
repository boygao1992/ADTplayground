module Selda.Table.Validation where

import Prelude

import Control.MonadZero (guard)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (any)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (Pattern(..))
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))
import Effect.Exception.Unsafe (unsafeThrow)

import Selda.Table.Type (ColAttr(..), ColInfo, Table(..), isPrimary, isUnique)
import Selda.Types (TableName, fromColName, fromTableName)

newtype ValidationError = ValidationError String
derive newtype instance eqValidationError :: Eq ValidationError
derive instance genericValidationError :: Generic ValidationError _
instance showValidationError :: Show ValidationError where show = genericShow
-- instance Exception ValidationError

-- | Ensure that there are no duplicate column names or primary keys.
--   Returns a list of validation errors encountered.
validate :: TableName -> Array ColInfo -> Array String
validate name cis = errs
  where
    colIdents = map (fromColName <<< _.colName) cis
    allIdents = fromTableName name : colIdents
    errs = Array.fold
      [ dupes
      , pkDupes
      , optionalRequiredMutex
      , nulIdents
      , emptyIdents
      , emptyTableName
      , nonPkFks
      ]
    emptyTableName
      | fromTableName name == "\"\"" = ["table name is empty"]
      | otherwise                    = []
    emptyIdents
      | any (_ == "\"\"") colIdents =
        ["table has columns with empty names"]
      | otherwise =
        []
    nulIdents = do
      n <- allIdents
      guard $ String.contains nul n
      pure $ "table or column name contains \\NUL: " <> n
      where
        -- NOTE '\NUL' in Haskell
        nul = Pattern $ String.singleton '\x0'
    dupes = do
      xs <- soup $ map _.colName cis
      let ({ head: x, tail }) = NEA.uncons xs
      if Array.length tail > 0
        then pure $ "duplicate column: " <> fromColName x
        else []
    pkDupes =
      if moreThanOne pkAttrs then ["multiple primary keys"] else []
    nonPkFks = do
      ci <- cis
      (Table {tableName: ftn, tableCols: fcs } /\ fcn) <- ci.colFKs
      fc <- fcs
      guard $ fc.colName == fcn
      guard $ not $ any isUnique fc.colAttrs
      pure $ "column is used as a foreign key, but is not primary or unique: "
        <> fromTableName ftn <> "." <> fromColName fcn

    -- This should be impossible, but...
    optionalRequiredMutex = do
      ci <- cis
      guard $ Optional `Array.elem` ci.colAttrs && Required `Array.elem` ci.colAttrs

      pure $ "BUG: column " <> fromColName ci.colName
                       <> " is both optional and required"

    moreThanOne []  = false
    moreThanOne [_] = false
    moreThanOne _   = true
    pkAttrs = do
      attr <- Array.concatMap _.colAttrs cis
      guard $ isPrimary attr
      pure attr


-- | Return all columns of the given table if the table schema is valid,
--   otherwise throw a 'ValidationError'.
validateOrThrow :: TableName -> Array ColInfo -> Array ColInfo
validateOrThrow name cols =
  case validate name cols of
    []     -> cols
    errors -> unsafeThrow $ show $ ValidationError $ Array.fold
      [ "validation of table `", fromTableName name
      , "' failed:\n  "
      , Array.intercalate "\n  " errors
      ]


-- | Sort a list and remove all duplicates from it.
snub :: forall a. Ord a => Array a -> Array a
snub = map NEA.head <<< soup

-- | Sort a list, then group all identical elements.
soup :: forall a. Ord a => Array a -> Array (NonEmptyArray a)
soup = Array.group <<< Array.sort
