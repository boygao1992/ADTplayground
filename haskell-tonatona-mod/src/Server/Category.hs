{-# LANGUAGE TypeOperators #-}
module Server.Category where

import RIO
import RIO.Partial (read)
import Servant
import Data.Bifunctor (bimap)

import Types (Resources)
import Server.Category.GetCategoryPaths (getCategoryPaths)

type Api
  = "batchValidate"
       :> ReqBody '[JSON] [(Text, String)]
       :> Post '[JSON] [(Text, [String])]

server :: ServerT Api (RIO Resources)
server
  = batchValidate

batchValidate :: [(Text, String)] -> RIO Resources [(Text, [String])]
batchValidate
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

