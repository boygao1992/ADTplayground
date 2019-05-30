module Server.Category.GetCategoryPaths where

import Types (Resources)

import RIO
import qualified Data.List.Util as List (returningOne)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.Util as Map

import Database.Beam.Query
import Database.Beam.MySQL.Connection (MySQLM)
import Tonatona.Beam.MySQL.Run (runBeamMySQL)

import Magento.Data.Categories (Category(..), Categories(..), leafNode, allLeafNodes)
import Magento.Data.CategoryPath (CategoryPath(..), unCategoryPath)
import Magento.Database
import qualified Magento.Database.Catalog.Category.Entity as CCE
import qualified Magento.Database.Catalog.Category.Entity.Varchar as CCEV

_getAllCandidateCategoryPathsByLeafNodeLabels :: [Text] -> MySQLM [(Text, CategoryPath)]
_getAllCandidateCategoryPathsByLeafNodeLabels leaves =
    fmap (mapMaybe \(x,y) -> (, CategoryPath . drop 1 . unCategoryPath $ y) <$> x)
    . runSelectReturningList
    $ select
    $ orderBy_ (asc_ . fst)
    $ nub_ do
        catalog_category_entity_varchar <-
          all_ (magentoDb^.catalogCategoryEntityVarchar)
        catalog_category_entity <-
          all_ (magentoDb^.catalogCategoryEntity)
        guard_ $ catalog_category_entity_varchar^.CCEV.value `in_` (fmap (just_ . val_) leaves)
        guard_ $ CCEV._row_id catalog_category_entity_varchar `references_` catalog_category_entity
        pure $ (catalog_category_entity_varchar^.CCEV.value
               , catalog_category_entity^.CCE.path
               )

_getCandidateLabelsByNodeIds :: [Word32] -> MySQLM [(Word32, Text)]
_getCandidateLabelsByNodeIds nodeIds =
    fmap (mapMaybe \(x,y) -> (x,) <$> y)
    . runSelectReturningList
    $ select
    $ nub_ do
        catalog_category_entity_varchar <-
          all_ (magentoDb^.catalogCategoryEntityVarchar)
        guard_
          $ catalog_category_entity_varchar^.CCEV.row_id
            `in_` (val_ <$> nodeIds)
        pure $
          ( catalog_category_entity_varchar^.CCEV.row_id
          , catalog_category_entity_varchar^.CCEV.value
          )

filterValidPaths :: Map.Map Word32 (Set.Set Text) -> Category -> [CategoryPath] -> [CategoryPath]
filterValidPaths nodeDict (Category category) =
  fmap CategoryPath
  . mapMaybe
    (\(CategoryPath path) ->
        for (path `zip` category) \(key, label) ->
          Map.lookup key nodeDict
          >>= \s -> if Set.member label s
                      then Just key
                      else Nothing
    )

categoryPathFromCategory :: Map.Map Text [CategoryPath] -> Category -> [CategoryPath]
categoryPathFromCategory nodeLabelToCategoryPaths category =
  fromMaybe [] do
    l <- leafNode category
    Map.lookup l nodeLabelToCategoryPaths

getCategoryPaths
  :: [(Text, Categories)]
  -> RIO Resources [(Text, [(Category, Maybe CategoryPath)])]
getCategoryPaths request = do
  let
    allLeafNodeLabels :: [Text]
      = allLeafNodes
      . fold
      . fmap snd
      $ request

  nodeLabelAndCategoryPathPairs :: [(Text, CategoryPath)]
    <- runBeamMySQL
    $ _getAllCandidateCategoryPathsByLeafNodeLabels allLeafNodeLabels

  let
    nodeLabelToCategoryPaths :: Map.Map Text [CategoryPath]
      = Map.fromListGroupBy nodeLabelAndCategoryPathPairs

    skuAndCategoryPathsPairs :: [(Text, [(Category, [CategoryPath])])]
      = fmap
        ( fmap
          ( fmap ( id &&& categoryPathFromCategory nodeLabelToCategoryPaths )
          . unCategories
          )
        )
      $ request

    allNodeIds :: [Word32]
      = Set.toList
      . Set.fromList
      . join
      . fmap unCategoryPath
      . fmap snd
      $ nodeLabelAndCategoryPathPairs

  allCandidateNodeLabels :: [(Word32, Text)]
    <- runBeamMySQL
    $ _getCandidateLabelsByNodeIds allNodeIds

  let
    nodeIdToCandidateNodeLabels :: Map.Map Word32 (Set Text)
      = fmap Set.fromList
      . Map.fromListGroupBy
      $ allCandidateNodeLabels

  -- TODO memorization to save computation for repeated categories
    result :: [(Text, [(Category, Maybe CategoryPath)])]
      = fmap
        ( fmap
          ( fmap
            (\pair ->
              ( List.returningOne
              . filterValidPaths nodeIdToCandidateNodeLabels (fst pair)
              ) <$> pair
            )
          )
        )
      $ skuAndCategoryPathsPairs

  pure result
