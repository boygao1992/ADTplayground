module Polaris.UI.Capability.Resource.Product where

import Prelude

import Halogen (HalogenM, lift)
import Data.Maybe (Maybe)
import Polaris.Data.Product (Products)

class Monad m <= ManageProduct m where
  getProducts :: m (Maybe Products)

instance manageProductHalogenM :: ManageProduct m => ManageProduct (HalogenM state action slots output m) where
  getProducts = lift getProducts
