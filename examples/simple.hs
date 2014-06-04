{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.LogFS (runLogFS, Packet(..))

import System.Fuse (defaultExceptionHandler)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan)
import Control.Monad (forever)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as B

usage :: IO ()
usage = do
 putStrLn $ "usage: ./simple <number of concurrent threads> <fuse options>"

runBackendStdout' :: Integer -> Chan Packet -> IO ()
runBackendStdout' n ch = do
 nch <- dupChan ch
 forever $ runBackendStdout n nch

runBackendStdout :: Integer -> Chan Packet -> IO ()
runBackendStdout n ch = do
 pkt <- readChan ch
 putStrLn $ "got("++show n++"): " ++ show pkt

runBackendFile' :: String -> Chan Packet -> IO ()
runBackendFile' file ch = do
 nch <- dupChan ch
 forever $ runBackendFile file nch

runBackendFile :: String -> Chan Packet -> IO ()
runBackendFile file ch = do
 pkt <- readChan ch
 appendFile file $ (B.unpack $ encode $ B.pack $ show pkt) ++ "\n"

launch :: Integer -> [String] -> IO ()
launch max' argv = do
 ch <- newChan
 mapM_ (\n -> forkIO $ runBackendStdout' n ch) [1..max']
 mapM_ (\n -> forkIO $ runBackendFile' ("/tmp/logfs."++show n++".log") ch) [1..max']
 let
  backendHandler :: Packet -> IO ()
  backendHandler pkt = do
   writeChan ch pkt

 runLogFS "logfs" argv backendHandler defaultExceptionHandler


main :: IO ()
main = do
 (tmax':argv) <- getArgs
 let tmax = read tmax' :: Integer
 case (tmax > 0) of
  False -> usage
  _ -> launch tmax argv
