module Magento.Import.Api.Sku.GetAllSkus where

import Types (Resources)

import RIO

import Database.Beam.Query
import Database.Beam.MySQL.Connection (MySQLM)
import Tonatona.Beam.MySQL.Run (runBeamMySQL)

import Magento.Data.Skus (Skus(..), Sku(..))
import Magento.Database
import qualified Magento.Database.Catalog.Product.Entity as CPE

_getAllSkus :: MySQLM [Text]
_getAllSkus =
  runSelectReturningList
  $ select
  $ nub_ do
    catalog_product_entity
      <- all_ (magentoDb^.catalogProductEntity)
    pure $ catalog_product_entity^.CPE.sku

getAllSkus :: RIO Resources Skus
getAllSkus = do
  allSkus <- runBeamMySQL _getAllSkus
  pure $ Skus . fmap Sku $ allSkus
