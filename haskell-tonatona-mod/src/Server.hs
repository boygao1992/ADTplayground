{-# LANGUAGE TypeOperators #-}
module Server where

import RIO
import Servant

import Types
import qualified Server.Category as Category
import qualified Server.Sku as Sku

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
