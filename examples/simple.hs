{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.LogFS (runLogFS, Packet(..))

import System.Fuse (defaultExceptionHandler)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as B

runBackendStdout :: Int -> Chan Packet -> IO ()
runBackendStdout n ch = do
 pkt <- readChan ch
 putStrLn $ "got("++show n++"): " ++ show pkt

runBackendFile :: Chan Packet -> IO ()
runBackendFile ch = do
 pkt <- readChan ch
 appendFile "/tmp/logfs.log" $ (B.unpack $ encode $ B.pack $ show pkt) ++ "\n"

launch :: [String] -> IO ()
launch argv = do
 ch <- newChan
 mapM_ (\n -> forkIO $ forever $ runBackendStdout n ch) [1..20]
 _ <- forkIO $ forever $ runBackendFile ch
 let
  backendHandler :: Packet -> IO ()
  backendHandler pkt = do
   writeChan ch pkt

 runLogFS "logfs" argv backendHandler defaultExceptionHandler


main :: IO ()
main = do
 argv <- getArgs
 launch argv
