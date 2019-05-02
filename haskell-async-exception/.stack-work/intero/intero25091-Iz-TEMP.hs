module Main2 where

import Prelude
import Control.Exception (SomeException, catch)
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)

catchAny :: forall a. IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

dangerous :: IO Int
dangerous = do
  threadDelay 10000
  putStrLn "Succeeds this time, but takes some time"
  return 5

worker :: IO ()
worker = do
  x <- catchAny dangerous $ \e -> do
    putStrLn $ "Caught an exception: " ++ show e
    putStrLn "Returning dummy value of -1"
    pure (-1)
  putStrLn $ "x + 10 == " ++ show (x + 10)


main :: IO ()
main = do
  res <- timeout 5000 worker
  case res of
    Nothing -> putStrLn "worker did not run to completion"
    Just () -> putStrLn "worker ran to completion"

{-
> Caught an exception: <<timeout>>
> Returning dummy value of -1
> x + 10 == 9
> worker ran to completion
-}
