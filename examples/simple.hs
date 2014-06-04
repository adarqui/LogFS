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
import qualified Data.PerfectHash as Hash
import Data.Maybe (isJust)

usage :: IO ()
usage = do
 putStrLn $ "usage: ./simple <number of concurrent threads> [suffixes,to,consider,as,directories] <fuse options>"

genList :: String -> [String]
genList s = read s :: [String]

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

launch :: Integer -> [String] -> [String] -> IO ()
launch max' suffixes argv = do
 ch <- newChan
 mapM_ (\n -> forkIO $ runBackendStdout' n ch) [1..max']
 mapM_ (\n -> forkIO $ runBackendFile' ("/tmp/logfs."++show n++".log") ch) [1..max']

 let
  hash :: Hash.PerfectHash Bool
  hash = Hash.fromList $ map (\x -> (B.pack x, True)) suffixes

 let
  backendHandler :: Packet -> IO ()
  backendHandler pkt = do
   writeChan ch pkt

  dirFilter :: String -> Bool
  dirFilter s = isJust $ Hash.lookup hash (B.pack s)

 runLogFS "logfs" argv backendHandler dirFilter defaultExceptionHandler


main :: IO ()
main = do
 (tmax':suffixes':argv) <- getArgs
 let tmax = read tmax' :: Integer
 let suffixes = genList suffixes'
 case (tmax > 0) of
  False -> usage
  _ -> launch tmax suffixes argv
