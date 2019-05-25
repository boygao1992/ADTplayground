{-# LANGUAGE TypeOperators #-}
module Server.CategoryValidation where

import RIO
import RIO.Partial (read)
import Servant
import Data.Bifunctor (bimap)

import Types (Resources)
import Server.CategoryValidation.GetCategoryPaths (getCategoryPaths)

type Api
  = "categories"
       :> ReqBody '[JSON] [(Text, String)]
       :> Post '[JSON] [(Text, [String])]

server :: ServerT Api (RIO Resources)
server
  = categories

categories :: [(Text, String)] -> RIO Resources [(Text, [String])]
categories
  = fmap
    ( filter (not . null . snd)
    . fmap
      ( fmap
        ( fmap fst
        . filter snd
        . fmap (bimap show isNothing)
        )
      )
    )
  . getCategoryPaths
  . fmap (fmap read)

