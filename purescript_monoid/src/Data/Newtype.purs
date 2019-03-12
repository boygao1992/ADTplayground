module Data.Newtype where

class Newtype t a | t -> a where
  pack :: a -> t
  unpack :: t -> a
