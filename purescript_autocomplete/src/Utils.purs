module Utils where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (filter)

import Halogen as H
import Halogen.HTML.Properties as HP


classList
  :: forall r i
   . Array (Tuple String Boolean)
  -> H.IProp ("class" :: String | r) i
classList = HP.classes <<< map (H.ClassName <<< fst) <<< filter snd

withCmds :: forall model cmd. model -> cmd -> Tuple model cmd
withCmds model cmd =
  Tuple model cmd

infixl 5 withCmds as !
