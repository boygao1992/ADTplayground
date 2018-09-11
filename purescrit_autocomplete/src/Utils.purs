module Utils where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (filter)
import Data.Foldable (fold)

import Halogen as H
import Halogen.HTML.Properties as HP


classList
  :: forall r i
   . Array (Tuple Boolean H.ClassName)
  -> Array (H.IProp ("class" :: String | r) i)
classList = map (HP.class_ <<< snd) <<< filter fst
