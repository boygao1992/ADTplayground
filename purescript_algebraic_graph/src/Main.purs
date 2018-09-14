module Main where

import Prelude

import Control.Monad.Free (Free, liftF)

data GraphF a next
  = Empty
  | Vertex a
  | Overlay next next
  | Connect next next

