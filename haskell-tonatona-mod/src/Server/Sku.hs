{-# LANGUAGE TypeOperators #-}
module Server.Sku where

import RIO
import Servant

import Types (Resources)
import Server.Sku.GetAllSkus (getAllSkus)

import Magento.Data.Skus (unSku, unSkus)

type Api
  = "full" :> Get '[JSON] [Text]

server :: ServerT Api (RIO Resources)
server
  = full

full :: RIO Resources [Text]
full = fmap unSku . unSkus <$> getAllSkus
