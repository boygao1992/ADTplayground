{-# LANGUAGE TypeOperators #-}
module Magento.Import.Api where

import RIO
import Servant

import Types
import qualified Magento.Import.Api.Category as Category
import qualified Magento.Import.Api.Sku as Sku

-- | Validation Endpoints

type RestApi
  = "category" :> Category.Api
  :<|> "sku" :> Sku.Api

restApiServer :: ServerT RestApi (RIO Resources)
restApiServer
  = Category.server
  :<|> Sku.server

-- | Serve Directories

type Api =
  RestApi
  :<|> Raw

server :: ServerT Api (RIO Resources)
server = restApiServer
  :<|> serveDirectoryFileServer "public"
