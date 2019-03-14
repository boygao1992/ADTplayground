module Model.Car where

import Prelude

import Data.Maybe (Maybe)

type CarSource =
  { model_id :: String
  , model_name :: String
  , model_trim :: String
  , make_display :: String
  , model_year :: Int
  }

type Car =
  { id :: String -- "model_id"
  , name :: String -- "model_name"
  , trim :: String -- "model_trim"
  , make :: String -- "make_display"
  , year :: Int -- "model_year"
  }

fromSource :: CarSource -> Car
fromSource
  { model_id
  , model_name
  , model_trim
  , make_display
  , model_year
  }
  =
  { id : model_id
  , name : model_name
  , trim : model_trim
  , make : make_display
  , year : model_year
  }
