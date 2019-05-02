module Main where

import Prelude
import Control.Exception (AsyncException, IOException, SomeException, catch, fromException, throwIO, throwTo, try)
import Control.Concurrent (forkIO, threadDelay)

catchAny :: forall a. IO a -> (SomeException -> IO a) -> IO a
catchAny m f = catch m onExc
  where
    onExc e
      | shouldCatch e = f e
      | otherwise = throwIO e
    shouldCatch e
      | show e == "<<timeout>>" = False
      | Just (_ :: AsyncException) <- fromException e = False
      | otherwise = True

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
  threadId <- forkIO worker
  eresult <- try $ readFile "does-not-exist.txt"
  case eresult of
    Left e -> throwTo threadId (e :: IOException)
    Right _ -> putStrLn "Funny, that shouldn't have worked"
  threadDelay 50000

{-
Caught an exception: does-not-exist.txt: openFile: does not exist (No such file or directory)
Returning dummy value of -1
x + 10 == 9
-}
