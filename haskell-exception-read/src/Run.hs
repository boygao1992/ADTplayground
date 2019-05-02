{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Text.ReadException

run :: RIO App ()
run = do
  logInfo . fromString $ "Enter an Int (non-runtime exception)"
  res1 <- readLine1
  logInfo . fromString . show $ (res1 :: Either SomeException Int)

  logInfo . fromString $ "Enter an Int (runtime exception)"
  res2 <- readLine2
  logInfo . fromString . show $ (res2 :: Int)
