{-# LANGUAGE TypeOperators #-}
module Server.Categories where

import RIO
import Servant
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import DB
import Types

import Magento.Data.Categories(Categories(..))

type CategoriesValidationApi
  = "category" :> ReqBody '[JSON] String :> Post '[JSON] Bool
  :<|> "categories" :> ReqBody '[JSON] String :> Post '[JSON] (Maybe [(String, Bool)])


category :: String -> RIO Resources Bool
category input = isJust <$> runMaybeT do
  c <- MaybeT $ pure $ readMaybe input
  MaybeT $ getCategoryPath $ c

categories :: String -> RIO Resources (Maybe [(String, Bool)])
categories input =
  case (readMaybe input :: Maybe Categories) of
    Nothing -> pure Nothing
    Just (Categories cs) -> Just <$>
      traverse (\c -> fmap (show c,) . fmap isJust . getCategoryPath $ c) cs

categoriesValidationApiServer :: ServerT CategoriesValidationApi (RIO Resources)
categoriesValidationApiServer
  = category
  :<|> categories
