{-# LANGUAGE TypeOperators #-}
module Magento.Import.Api.Category where

import RIO
import Servant
import Data.Bifunctor as Bifunctor (second)

import Magento.Types (Resources)
import Magento.Data.Skus (Sku)
import Magento.Data.Categories (Categories(..))
import Magento.Import.Api.Category.GetCategoryPaths (getCategoryPaths)

type Api
  = "batchValidate"
       :> ReqBody '[JSON] [(Sku, Categories)]
       :> Post '[JSON] [(Sku, Categories)]

server :: ServerT Api (RIO Resources)
server
  = batchValidate

batchValidate :: [(Sku, Categories)] -> RIO Resources [(Sku, Categories)]
batchValidate
  = fmap
    ( fmap (fmap Categories)
    . filter (not . null . snd)
    . fmap
      (fmap
        ( fmap fst
        . filter snd
        . fmap (Bifunctor.second isNothing)
        )
      )
    )
  . getCategoryPaths

