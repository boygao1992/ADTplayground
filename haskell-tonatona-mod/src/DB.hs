{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}

module DB where

import Types (Resources)

import RIO
import RIO.List (nub, groupBy, headMaybe, lastMaybe, drop, length)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Database.Beam.Query
import Database.Beam.MySQL.Connection (MySQLM)
import Tonatona.Beam.MySQL.Run (runBeamMySQL)

import Magento.Data.Categories (Category(..))
import Magento.Data.CategoryPath (CategoryPath(..), unCategoryPath)
import Magento.Database
import qualified Magento.Database.Eav.Entity.Type as EET
import qualified Magento.Database.Eav.Attribute as EA
import qualified Magento.Database.Eav.Attribute.Option as EAO
import qualified Magento.Database.Eav.Attribute.Option.Value as EAOV
import qualified Magento.Database.Catalog.Category.Entity as CCE
import qualified Magento.Database.Catalog.Category.Entity.Varchar as CCEV

_getAllAttributeOptionValuesByAttributeCode :: String -> MySQLM [Maybe Text]
_getAllAttributeOptionValuesByAttributeCode my_attribute_code =
  runSelectReturningList $ select do
    eav_attribute_option_value <-
      filter_ (\table -> table^.EAOV.value /=. nothing_)
      $ all_ (magentoDb^.eavAttributeOptionValue)
    eav_attribute_option <- all_ (magentoDb^.eavAttributeOption)
    eav_attribute <- all_ (magentoDb^.eavAttribute)
    guard_ $ EAOV._option_id eav_attribute_option_value `references_` eav_attribute_option
    guard_ $ EAO._attribute_id eav_attribute_option `references_` eav_attribute
    guard_ $ eav_attribute^.EA.attribute_code ==. just_ (fromString my_attribute_code)
    pure $ eav_attribute_option_value^.EAOV.value

_getAttributeMetaData :: String -> String -> MySQLM (Maybe (EA.EavAttribute, EET.EavEntityType))
_getAttributeMetaData my_attribute_code my_entity_type_code =
  let
    attribute_code = fromString my_attribute_code
    entity_type_code = fromString my_entity_type_code
  in
    runSelectReturningOne
    $ select
    $ nub_ do
        eav_attribute <- all_ (magentoDb^.eavAttribute)
        eav_entity_type <- all_ (magentoDb^.eavEntityType)
        guard_ $ EA._entity_type_id eav_attribute `references_` eav_entity_type
        guard_ $ eav_attribute^.EA.attribute_code ==. just_ attribute_code
        guard_ $ eav_entity_type^.EET.entity_type_code ==. entity_type_code
        pure $
          ( eav_attribute
          , eav_entity_type
          )

_getCandidateCategoryPathsByLeafNodeLabel :: Text -> MySQLM [CategoryPath]
_getCandidateCategoryPathsByLeafNodeLabel leaf =
    runSelectReturningList
    $ select
    $ nub_ do
        catalog_category_entity_varchar <-
          all_ (magentoDb^.catalogCategoryEntityVarchar)
        catalog_category_entity <-
          all_ (magentoDb^.catalogCategoryEntity)
        guard_ $ catalog_category_entity_varchar^.CCEV.value ==. just_ (val_ leaf)
        guard_ $ CCEV._row_id catalog_category_entity_varchar `references_` catalog_category_entity
        pure $ catalog_category_entity^.CCE.path

_getCandidateLabelsByNodeIds :: [Word32] -> MySQLM [(Word32, Maybe Text)]
_getCandidateLabelsByNodeIds nodeIds =
    runSelectReturningList
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

nodeDictFromList :: [(Word32, Maybe Text)] -> Map.Map Word32 (Set.Set Text)
nodeDictFromList
  = Map.fromList
  . mapMaybe (\(x,y) -> (,y) <$> x)
  . fmap ( (fmap fst . headMaybe) &&& (Set.fromList . fmap snd) )
  . groupBy (\x y -> fst x == fst y)
  . mapMaybe (\(x,y) -> (x,) <$> y)

filterValidPaths :: Map.Map Word32 (Set.Set Text) -> [Text] -> [[Word32]] -> [[Word32]]
filterValidPaths nodeDict category =
  mapMaybe
  (\path ->
      for (path `zip` category) \(key, label) ->
        Map.lookup key nodeDict
        >>= \s -> if Set.member label s
                    then Just key
                    else Nothing
  )

getCategoryPath :: Category -> RIO Resources (Maybe (CategoryPath))
getCategoryPath (Category category) = do
  let
    leaf :: Maybe Text
    leaf = lastMaybe category

  result :: [CategoryPath] <- runBeamMySQL
    $ runSelectReturningList
    $ select
    $ nub_ do
        catalog_category_entity_varchar <-
          all_ (magentoDb^.catalogCategoryEntityVarchar)
        catalog_category_entity <-
          all_ (magentoDb^.catalogCategoryEntity)
        guard_ $ catalog_category_entity_varchar^.CCEV.value ==. val_ leaf
        guard_ $ CCEV._row_id catalog_category_entity_varchar `references_` catalog_category_entity
        pure $ catalog_category_entity^.CCE.path

  let
    paths :: [[Word32]]
    paths = drop 1 . unCategoryPath <$> result

    nodeIds :: [Word32]
    nodeIds = nub . join $ paths

  nodes :: [(Word32, Maybe Text)] <-
    runBeamMySQL $ _getCandidateLabelsByNodeIds nodeIds

  let
    nodeDict :: Map.Map Word32 (Set.Set Text)
    nodeDict
      = Map.fromList
      . mapMaybe (\(x,y) -> (,y) <$> x)
      . fmap ( (fmap fst . headMaybe) &&& (Set.fromList . fmap snd) )
      . groupBy (\x y -> fst x == fst y)
      . mapMaybe (\(x,y) -> (x,) <$> y)
      $ nodes

  let
    validPaths :: [[Word32]]
    validPaths = filterValidPaths nodeDict category paths

  pure $ CategoryPath <$>
    if length validPaths /= 1
      then Nothing
      else headMaybe validPaths
