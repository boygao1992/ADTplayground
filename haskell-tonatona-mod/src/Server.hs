{-# LANGUAGE TypeOperators #-}
module Server where

import RIO
import Servant

import Types
import qualified Server.CategoryValidation as CategoryValidation

-- | Validation Endpoints

type RestApi
  = CategoryValidation.Api

restApiServer :: ServerT RestApi (RIO Resources)
restApiServer
  = CategoryValidation.server

-- | Serve Directories

type Api =
  RestApi
  :<|> Raw

server :: ServerT Api (RIO Resources)
server = restApiServer
  :<|> serveDirectoryFileServer "public"
