module Selda.SQL.Print where

import Prelude

import Control.Monad.State
import Data.Array as Array
import Data.Array.NonEmpty as NEA

import Selda.Column
import Selda.SQL
import Selda.SQL.Print.Config (PPConfig)
import Selda.SQL.Print.Config as Cfg
import Selda.SqlType
import Selda.Types

snub :: forall a. Ord a => Array a -> Array a
snub = map NEA.head <<< Array.group <<< Array.sort

-- | SQL pretty-printer. The state is the list of SQL parameters to the
--   prepared statement.
type PP = State PPState

newtype PPState = PPState
  { ppParams  :: Array Param
  , ppParamNS :: Int
  , ppQueryNS :: Int
  , ppConfig  :: PPConfig
  }
