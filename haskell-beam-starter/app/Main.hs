{-# LANGUAGE
OverloadedStrings
#-}

module Main where

import Prelude
import Lib (UserT(User), User)

main :: IO ()
main = do
  putStrLn . show
    $ (User "john@example.com" "John" "Smith" "password!" :: User)
