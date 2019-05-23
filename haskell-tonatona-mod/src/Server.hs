{-# LANGUAGE TypeOperators #-}
module Server where

import RIO
import Servant

import Types
import Server.Categories


-- | Validation Endpoints

type RestApi
  = CategoriesValidationApi

restApiServer :: ServerT RestApi (RIO Resources)
restApiServer
  = categoriesValidationApiServer

-- | Serve Directories

type Api =
  RestApi
  :<|> Raw

apiServer :: ServerT Api (RIO Resources)
apiServer = restApiServer
  :<|> serveDirectoryFileServer "public"
