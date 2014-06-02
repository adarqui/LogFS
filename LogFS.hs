module Main where

import System.LogFS (runLogFS)
import System.LogFS.Run (runBackend)
import System.Fuse (fuseMain, defaultExceptionHandler)
import System.DevUtils.Data.List (split)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (writeChan)
import Control.Monad (forever)

usage :: IO ()
usage = do
 putStrLn "usage: ./logfs logfs options ::: fuse options"

launch :: [String] -> [String] -> IO ()
launch logfs fuse = do

 ch <- runBackend logfs

 let
  backendHandler :: String -> IO ()
  backendHandler s = do
   writeChan ch s

 runLogFS "logfs" fuse backendHandler defaultExceptionHandler

main :: IO ()
main = do
 argv <- getArgs
 case (split ":::" argv) of
  (logfs:fuse:[]) -> launch logfs fuse
  _ -> usage
